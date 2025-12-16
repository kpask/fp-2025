{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Lib3 (
    State(..),
    Song(..),
    Playlist(..),
    emptyState,
    applyCommand,
    applyCommandR,
    storageOpLoop,
    StorageOp(..),
    load,
    save,
    execute,
    renderPlaylist,
    renderPlayback,
    totalDuration,
    Parser(..),
    parseCommand
) where

import qualified Lib2
import qualified Lib1
import Lib1 (Command)
import Control.Exception (catch, IOException)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (isSpace)
import Prelude hiding (readFile, writeFile)
import System.IO.Strict (readFile)
import System.IO (writeFile)

-- ============================================================
-- PARSER
-- ============================================================

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input ->
            case p input of
                Left e -> Left e
                Right (a, rest) -> Right (f a, rest)

instance Applicative Parser where
    pure x = Parser $ \inp -> Right (x, inp)
    (Parser pf) <*> (Parser pa) =
        Parser $ \inp ->
            case pf inp of
                Left e -> Left e
                Right (f, rest) ->
                    case pa rest of
                        Left e -> Left e
                        Right (a, rest') -> Right (f a, rest')

instance Alternative Parser where
    empty = Parser $ const (Left "parse failed")
    (Parser p1) <|> (Parser p2) =
        Parser $ \inp ->
            case p1 inp of
                Right ok -> Right ok
                Left _ -> p2 inp

-- ============================================================
-- PRIMITIVES
-- ============================================================

parseChar :: Char -> Parser Char
parseChar c = Parser $ \inp ->
    case inp of
        (x:xs) | x == c -> Right (c, xs)
        _ -> Left $ "Expected '" ++ [c] ++ "'"

parseString :: String -> Parser String
parseString = traverse parseChar

parseWhitespace :: Parser String
parseWhitespace = Parser $ \inp ->
    let (ws, rest) = span isSpace inp
    in Right (ws, rest)

parseNumber :: Parser Int
parseNumber = read <$> some (Parser f)
  where
    f (x:xs) | x >= '0' && x <= '9' = Right (x, xs)
    f _ = Left "Expected digit"

parseQuotedString :: Parser String
parseQuotedString =
    parseChar '"' *> Parser go
  where
    go inp =
        let (txt, rest) = break (== '"') inp
        in case rest of
            [] -> Left "Unterminated string"
            (_:xs) -> Right (txt, xs)

-- ============================================================
-- COMMAND PARSER
-- ============================================================

parseDump :: Parser Lib1.Command
parseDump =
    Lib1.Dump <$> (parseString "dump" *> parseWhitespace *> parseString "examples" *> pure Lib1.Examples)

parseCreate :: Parser Lib1.Command
parseCreate =
    Lib1.CreatePlaylist <$> (parseString "create playlist" *> parseWhitespace *> parseQuotedString)

parseAddSong :: Parser Lib1.Command
parseAddSong =
    (\(t,a,d) p -> Lib1.AddSong t a d p)
        <$> (parseString "add song" *> parseWhitespace *> song)
        <*> (parseWhitespace *> parseString "to playlist" *> parseWhitespace *> parseQuotedString)
  where
    song =
        (\t _ a _ d -> (t,a,d))
            <$> parseQuotedString
            <*> (parseWhitespace *> parseString "by" *> parseWhitespace)
            <*> parseQuotedString
            <*> parseWhitespace
            <*> parseNumber

parseAddPlaylist :: Parser Lib1.Command
parseAddPlaylist =
    Lib1.AddPlaylist
        <$> (parseString "add playlist" *> parseWhitespace *> parseQuotedString)
        <*> (parseWhitespace *> parseString "to playlist" *> parseWhitespace *> parseQuotedString)

parseShow :: Parser Lib1.Command
parseShow =
    Lib1.ShowPlaylist <$> (parseString "show playlist" *> parseWhitespace *> parseQuotedString)

parseTotal :: Parser Lib1.Command
parseTotal =
    Lib1.TotalDuration <$> (parseString "total duration of playlist" *> parseWhitespace *> parseQuotedString)

parsePlay :: Parser Lib1.Command
parsePlay =
    Lib1.PlayPlaylist <$> (parseString "play playlist" *> parseWhitespace *> parseQuotedString)

parseCommand :: Parser Lib1.Command
parseCommand =
        parseDump
    <|> parseCreate
    <|> parseAddSong
    <|> parseAddPlaylist
    <|> parseShow
    <|> parseTotal
    <|> parsePlay

-- ============================================================
-- DOMAIN
-- ============================================================

data Song = Song {
    songTitle :: String,
    songArtist :: String,
    songDuration :: Int
} deriving (Eq, Show)

data Playlist = Playlist {
    playlistName :: String,
    playlistSongs :: [Song],
    playlistNested :: [String]
} deriving (Eq, Show)

newtype State = State {
    playlists :: Map String Playlist
} deriving Show

emptyState :: State
emptyState = State Map.empty

-- ============================================================
-- PURE BUSINESS LOGIC
-- ============================================================

-- Simple version that only returns new state
applyCommand :: Lib1.Command -> State -> Either String State
applyCommand cmd st = snd <$> applyCommandR cmd st

-- Full version that returns both output and new state
applyCommandR :: Lib1.Command -> State -> Either String (Maybe String, State)
applyCommandR (Lib1.CreatePlaylist n) st
    | Map.member n (playlists st) =
        Left $ "Playlist '" ++ n ++ "' already exists"
    | otherwise =
        let st' = st { playlists = Map.insert n (Playlist n [] []) (playlists st) }
        in Right (Nothing, st')

applyCommandR (Lib1.AddSong t a d p) st =
    case Map.lookup p (playlists st) of
        Nothing -> Left $ "Playlist '" ++ p ++ "' not found"
        Just pl ->
            let pl' = pl { playlistSongs = playlistSongs pl ++ [Song t a d] }
                st' = st { playlists = Map.insert p pl' (playlists st) }
            in Right (Nothing, st')

applyCommandR (Lib1.AddPlaylist n p) st =
    case (Map.lookup n (playlists st), Map.lookup p (playlists st)) of
        (Nothing, _) -> Left $ "Nested playlist '" ++ n ++ "' not found"
        (_, Nothing) -> Left $ "Parent playlist '" ++ p ++ "' not found"
        (Just _, Just pl) ->
            let pl' = pl { playlistNested = playlistNested pl ++ [n] }
                st' = st { playlists = Map.insert p pl' (playlists st) }
            in Right (Nothing, st')

applyCommandR (Lib1.ShowPlaylist n) st =
    case Map.lookup n (playlists st) of
        Nothing -> Left $ "Playlist not found: " ++ n
        Just pl -> Right (Just (renderPlaylist pl), st)

applyCommandR (Lib1.TotalDuration n) st =
    case Map.lookup n (playlists st) of
        Nothing -> Left $ "Playlist not found: " ++ n
        Just pl -> Right (Just $ show $ totalDuration pl, st)

applyCommandR (Lib1.PlayPlaylist n) st =
    case Map.lookup n (playlists st) of
        Nothing -> Left $ "Playlist not found: " ++ n
        Just pl -> Right (Just (renderPlayback pl), st)

applyCommandR (Lib1.Dump Lib1.Examples) st =
    Right (Just $ unlines (map Lib2.toCliCommand Lib1.examples), st)

-- ============================================================
-- EXECUTE (IO SHELL)
-- ============================================================

execute :: TVar State -> Lib1.Command -> IO ()
execute stVar cmd = do
    res <- atomically $ do
        st <- readTVar stVar
        case applyCommandR cmd st of
            Left e -> return (Left e)
            Right (mout, st') -> writeTVar stVar st' >> return (Right (mout, st'))
    case res of
        Left err -> putStrLn $ "Error: " ++ err
        Right (mout, _) ->
            case mout of
                Nothing -> putStrLn "Command executed."
                Just out -> putStrLn out

-- ============================================================
-- STORAGE THREAD
-- ============================================================

data StorageOp = Save String (Chan ()) | Load (Chan String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop ch = loop
  where
    loop = do
        op <- readChan ch
        case op of
            Save txt done -> do
                writeFile "playlist_state.txt" txt
                writeChan done ()
                loop
            Load res -> do
                txt <- readFile "playlist_state.txt" `catch` handleReadError
                writeChan res txt
                loop

handleReadError :: IOException -> IO String
handleReadError _ = return ""

-- ============================================================
-- SAVE / LOAD
-- ============================================================

stateToCommands :: State -> [String]
stateToCommands st = concatMap one (Map.elems $ playlists st)
  where
    one pl =
        ["create playlist \"" ++ playlistName pl ++ "\""]
        ++ [ "add song \"" ++ songTitle s ++ "\" by \"" ++ songArtist s ++ "\" "
             ++ show (songDuration s) ++ " to playlist \"" ++ playlistName pl ++ "\""
           | s <- playlistSongs pl ]
        ++ [ "add playlist \"" ++ n ++ "\" to playlist \"" ++ playlistName pl ++ "\""
           | n <- playlistNested pl ]

save :: Chan StorageOp -> TVar State -> IO (Either String ())
save ch stVar = do
    st <- atomically (readTVar stVar)
    done <- newChan
    writeChan ch (Save (unlines (stateToCommands st)) done)
    readChan done
    return (Right ())

load :: Chan StorageOp -> TVar State -> IO (Either String ())
load ch stVar = do
    res <- newChan
    writeChan ch (Load res)
    txt <- readChan res
    let cmds = filter (not . null) (lines txt)
        parsed = traverse (fmap fst . runParser parseCommand) cmds
    case parsed of
        Left e -> return (Left e)
        Right cs -> do
            atomically (writeTVar stVar (foldl step emptyState cs))
            return (Right ())
  where
    step st c = either (const st) snd (applyCommandR c st)

-- ============================================================
-- HELPERS TO RENDER PLAYLISTS
-- ============================================================

renderPlaylist :: Playlist -> String
renderPlaylist pl =
    "Playlist: " ++ playlistName pl ++ "\n" ++
    "Songs: " ++ show (length (playlistSongs pl)) ++ "\n" ++
    unlines (map renderSong (playlistSongs pl)) ++
    if null (playlistNested pl) then ""
    else "Nested playlists: " ++ unwords (playlistNested pl)

renderPlayback :: Playlist -> String
renderPlayback pl =
    "Playing playlist: " ++ playlistName pl ++ "\n" ++
    unlines (map (\s -> "♪ " ++ songTitle s ++ " - " ++ songArtist s) (playlistSongs pl))

renderSong :: Song -> String
renderSong s = "  - " ++ songTitle s ++ " by " ++ songArtist s ++ " (" ++ show (songDuration s) ++ "s)"

totalDuration :: Playlist -> Int
totalDuration pl = sum $ map songDuration (playlistSongs pl)