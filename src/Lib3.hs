{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop,
    StorageOp, Parser(..), parseCommand
) where

import qualified Lib1
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent (Chan, readChan, newChan, writeChan)
import System.IO.Strict (readFile)
import System.IO (writeFile)
import Control.Exception (catch)
import Control.Applicative (Alternative(..))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (isSpace)
import Prelude hiding (readFile, writeFile)

-- ============================================================
-- PARSER TYPE
-- ============================================================

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input ->
            case p input of
                Left err -> Left err
                Right (a, rest) -> Right (f a, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Right (x, input)

    (Parser pf) <*> (Parser pa) =
        Parser $ \input ->
            case pf input of
                Left err -> Left err
                Right (f, rest1) ->
                    case pa rest1 of
                        Left err -> Left err
                        Right (a, rest2) -> Right (f a, rest2)

instance Alternative Parser where
    empty = Parser $ \_ -> Left "parse failed"

    (Parser p1) <|> (Parser p2) =
        Parser $ \input ->
            case p1 input of
                Right ok -> Right ok
                Left _   -> p2 input

-- ============================================================
-- PRIMITIVE PARSERS
-- ============================================================

parseChar :: Char -> Parser Char
parseChar c =
    Parser $ \inp ->
        case inp of
            [] -> Left $ "Expected '" ++ [c] ++ "', but got end of input"
            (x:xs) | x == c -> Right (c, xs)
                   | otherwise -> Left $ "Expected '" ++ [c] ++ "', got '" ++ [x] ++ "'"

parseString :: String -> Parser String
parseString = traverse parseChar

parseWhitespace :: Parser String
parseWhitespace =
    Parser $ \inp ->
        let (ws, rest) = span isSpace inp
        in Right (ws, rest)

parseDigit :: Parser Char
parseDigit =
    Parser $ \inp ->
        case inp of
            (x:xs) | x >= '0' && x <= '9' -> Right (x, xs)
            _ -> Left "Expected digit"

parseNumber :: Parser Int
parseNumber = read <$> some parseDigit

parseQuotedString :: Parser String
parseQuotedString =
    parseChar '"' *> go
  where
    go = Parser $ \inp ->
        let (content, rest) = break (== '"') inp
        in case rest of
            [] -> Left "Unterminated string literal"
            (_:xs) -> Right (content, xs)

-- ============================================================
-- DOMAIN PARSERS
-- ============================================================

parseDumpable :: Parser Lib1.Dumpable
parseDumpable = parseString "examples" *> pure Lib1.Examples

parseSong :: Parser (String, String, Int)
parseSong =
    (\t _ a _ d -> (t,a,d))
        <$> parseQuotedString
        <*> (parseWhitespace *> parseString "by" *> parseWhitespace)
        <*> parseQuotedString
        <*> parseWhitespace
        <*> parseNumber

-- ============================================================
-- COMMAND PARSERS
-- ============================================================

parseDump :: Parser Lib1.Command
parseDump = Lib1.Dump <$> (parseString "dump" *> parseWhitespace *> parseDumpable)

parseCreatePlaylist :: Parser Lib1.Command
parseCreatePlaylist =
    Lib1.CreatePlaylist <$> (parseString "create playlist" *> parseWhitespace *> parseQuotedString)

parseAddSong :: Parser Lib1.Command
parseAddSong =
    (\(t,a,d) playlist -> Lib1.AddSong t a d playlist)
        <$> (parseString "add song" *> parseWhitespace *> parseSong)
        <*> (parseWhitespace *> parseString "to playlist" *> parseWhitespace *> parseQuotedString)

parseAddPlaylist :: Parser Lib1.Command
parseAddPlaylist =
    (\nested parent -> Lib1.AddPlaylist nested parent)
        <$> (parseString "add playlist" *> parseWhitespace *> parseQuotedString)
        <*> (parseWhitespace *> parseString "to playlist" *> parseWhitespace *> parseQuotedString)

parseShowPlaylist :: Parser Lib1.Command
parseShowPlaylist =
    Lib1.ShowPlaylist <$> (parseString "show playlist" *> parseWhitespace *> parseQuotedString)

parseTotalDuration :: Parser Lib1.Command
parseTotalDuration =
    Lib1.TotalDuration <$> (parseString "total duration of playlist" *> parseWhitespace *> parseQuotedString)

parsePlayPlaylist :: Parser Lib1.Command
parsePlayPlaylist =
    Lib1.PlayPlaylist <$> (parseString "play playlist" *> parseWhitespace *> parseQuotedString)

parseCommand :: Parser Lib1.Command
parseCommand =
    parseDump
    <|> parseCreatePlaylist
    <|> parseAddSong
    <|> parseAddPlaylist
    <|> parseShowPlaylist
    <|> parseTotalDuration
    <|> parsePlayPlaylist

-- ============================================================
-- DATA TYPES
-- ============================================================

data Song = Song {
    songTitle :: String,
    songArtist :: String,
    songDuration :: Int
} deriving (Show, Eq)

data Playlist = Playlist {
    playlistName :: String,
    playlistSongs :: [Song],
    playlistNested :: [String]
} deriving (Show, Eq)

newtype State = State {
    playlists :: Map String Playlist
} deriving (Show)

emptyState :: State
emptyState = State Map.empty

-- ============================================================
-- BUSINESS LOGIC
-- ============================================================

-- If the playlist already exists → error. otherwise: Insert a new empty playlist into the map.
applyCommand :: Lib1.Command -> State -> Either String State
applyCommand (Lib1.CreatePlaylist name) st =
    if Map.member name (playlists st)
    then Left $ "Playlist '" ++ name ++ "' already exists"
    else Right st{ playlists = Map.insert name (Playlist name [] []) (playlists st) }

applyCommand (Lib1.AddSong t a d plName) st =
    case Map.lookup plName (playlists st) of
        Nothing -> Left $ "Playlist '" ++ plName ++ "' not found"
        Just pl ->
            let newPl = pl { playlistSongs = playlistSongs pl ++ [Song t a d] }
            in Right st { playlists = Map.insert plName newPl (playlists st) }

applyCommand (Lib1.AddPlaylist nested parent) st =
    case (Map.lookup nested (playlists st), Map.lookup parent (playlists st)) of
        (Nothing, _) -> Left $ "Nested playlist '" ++ nested ++ "' not found"
        (_, Nothing) -> Left $ "Parent playlist '" ++ parent ++ "' not found"
        (Just _, Just pl) ->
            let newPl = pl { playlistNested = playlistNested pl ++ [nested] }
            in Right st { playlists = Map.insert parent newPl (playlists st) }

applyCommand _ st = Right st

formatDuration :: Int -> String
formatDuration secs =
    let m = secs `div` 60
        s = secs `mod` 60
        ss = if s < 10 then '0' : show s else show s
    in show m ++ ":" ++ ss

-- ============================================================
-- EXECUTE
-- ============================================================

-- Execute a command: updates state atomically and displays result
-- Takes: thread-safe state variable and a command to execute
-- Returns: IO action that performs the command
execute :: TVar State -> Lib1.Command -> IO ()
execute stVar cmd = do
    -- Step 1: Atomically read state, apply command, and write new state
    result <- atomically $ do
        -- Read current state from the thread-safe variable
        st <- readTVar stVar
        
        -- Try to apply the command (pure function - no side effects)
        case applyCommand cmd st of
            -- If command failed (e.g., playlist not found)
            Left err -> return (Left err)  -- Return error wrapped in Either
            
            -- If command succeeded
            Right newSt -> 
                writeTVar stVar newSt       -- Write new state to TVar
                >> return (Right newSt)     -- And return the new state
    
    -- Step 2: Handle the result - either print error or show view
    either 
        (\err -> putStrLn $ "Error: " ++ err)  -- If Left: print error message
        printView                               -- If Right: display command output
        result
  where
    -- Helper function to display output based on command type
    printView st =
        case cmd of
            -- Display playlist contents
            Lib1.ShowPlaylist name ->
                -- Look up playlist by name in the state
                case Map.lookup name (playlists st) of
                    -- Playlist doesn't exist
                    Nothing -> putStrLn "(playlist not found)"
                    
                    -- Playlist found - display it
                    Just pl -> do
                        -- Print playlist header
                        putStrLn $ "Playlist: " ++ name
                        putStrLn $ "Songs (" ++ show (length (playlistSongs pl)) ++ "):"
                        
                        -- Print each song: "- Title by Artist (MM:SS)"
                        mapM_ (\s ->
                            putStrLn (" - " ++ songTitle s
                                       ++ " by " ++ songArtist s
                                       ++ " (" ++ formatDuration (songDuration s) ++ ")")
                            ) (playlistSongs pl)
            
            -- Calculate and display total playlist duration
            Lib1.TotalDuration name ->
                -- Look up playlist by name
                case Map.lookup name (playlists st) of
                    Nothing -> putStrLn "(playlist not found)"
                    Just pl -> 
                        -- Sum all song durations and display
                        putStrLn $ "Total duration: " 
                                ++ show (sum $ map songDuration (playlistSongs pl)) 
                                ++ "s"
            
            -- Play playlist (display songs with music note)
            Lib1.PlayPlaylist name ->
                -- Look up playlist by name
                case Map.lookup name (playlists st) of
                    Nothing -> putStrLn "(playlist not found)"
                    Just pl -> 
                        -- Print each song: "♪ Title - Artist"
                        mapM_ (\s -> putStrLn ("♪ " ++ songTitle s ++ " - " ++ songArtist s))
                              (playlistSongs pl)
            
            -- For all other commands (create, add, etc.) - just confirm execution
            _ -> putStrLn "Command executed."

-- ============================================================
-- STORAGE
-- ============================================================

-- Message type for communicating with storage thread
-- Save: write content to file, respond on channel when done
-- Load: read file content, send it back on channel
data StorageOp = Save String (Chan ()) | Load (Chan String)

-- Storage thread loop - handles file I/O operations
-- Runs forever in a dedicated thread, processing save/load requests
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop ch = loop
  where
    loop = do
        -- Block and wait for next storage operation message
        op <- readChan ch
        
        case op of
            -- Handle save request
            Save content done -> do
                -- Write content to disk
                writeFile "playlist_state.txt" content
                
                -- Signal completion back to caller
                writeChan done ()
                
                -- Continue loop (wait for next message)
                loop
            
            -- Handle load request
            Load res -> do
                -- Read file (or empty string if file doesn't exist)
                txt <- readFile "playlist_state.txt" `catch` handleNotFound
                
                -- Send content back to caller
                writeChan res txt
                
                -- Continue loop (wait for next message)
                loop

    -- Error handler: if file not found, return empty string
    handleNotFound :: IOError -> IO String
    handleNotFound _ = return ""

-- Convert in-memory State to list of CLI commands
-- This allows us to save state as human-readable commands
stateToCommands :: State -> [String]
stateToCommands st = concatMap one (Map.elems $ playlists st)
                     --         ^    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
                     --         |    Get all playlists as a list
                     --         Apply 'one' to each, flatten results
  where
    -- Generate commands for a single playlist
    one pl =
        -- Create the playlist
        ["create playlist \"" ++ playlistName pl ++ "\""]
        
        -- Add all songs (list comprehension generates one command per song)
        ++ [ "add song \"" ++ songTitle s ++ "\" by \"" ++ songArtist s ++ "\" "
             ++ show (songDuration s) ++ " to playlist \"" ++ playlistName pl ++ "\""
           | s <- playlistSongs pl ]  -- For each song in the playlist
        
        -- Add nested playlist references
        ++ [ "add playlist \"" ++ n ++ "\" to playlist \"" ++ playlistName pl ++ "\""
           | n <- playlistNested pl ]  -- For each nested playlist name

-- Save current state to disk
-- Converts state to commands and sends to storage thread
save :: Chan StorageOp -> TVar State -> IO (Either String ())
save ch stVar = do
    -- Read current state from TVar (atomically)
    st <- atomically (readTVar stVar)
    
    -- Convert state to command strings and join with newlines
    let txt = unlines (stateToCommands st)
    
    -- Create response channel to wait for completion
    done <- newChan
    
    -- Send save request to storage thread
    writeChan ch (Save txt done)
    
    -- Block until storage thread signals completion
    _ <- readChan done
    
    -- Return success
    return (Right ())

-- Load state from disk
-- Reads file, parses commands, and reconstructs state
load :: Chan StorageOp -> TVar State -> IO (Either String ())
load ch stVar = do
    -- Create response channel for file content
    res <- newChan
    
    -- Send load request to storage thread
    writeChan ch (Load res)
    
    -- Block and wait for file content
    content <- readChan res

    -- Split content into lines and remove empty lines
    let cmds = filter (not . null) (lines content)
        
        -- Parse each line as a command
        -- traverse: parse all lines, fail if any parse fails
        -- fmap fst: extract just the Command, discard remaining input
        parsed = traverse (fmap fst . runParser parseCommand) cmds

    -- Handle parse result
    case parsed of
        -- If any command failed to parse, return error
        Left err -> return (Left err)
        
        -- If all parsed successfully, reconstruct state
        Right cmdList -> do
            -- Apply commands sequentially, starting from empty state
            -- foldl: accumulate state by applying each command
            -- run: helper that applies command, ignoring errors
            let newState = foldl run emptyState cmdList
            
            -- Write reconstructed state to TVar (atomically)
            atomically (writeTVar stVar newState)
            
            -- Return success
            return (Right ())
  where
    -- Helper: apply a single command to state
    run st cmd =
        case applyCommand cmd st of
            -- If command fails, keep old state (graceful degradation)
            Left _ -> st
            
            -- If command succeeds, use new state
            Right st' -> st'