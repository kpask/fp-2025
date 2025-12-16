{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module Lib4 (
    parseCommand,
    PlaylistF(..),
    PlaylistDSL,
    createPlaylist,
    addSong,
    addPlaylist,
    showPlaylist,
    totalDuration,
    playPlaylist,
    dumpExamples,
    runHTTP,
    runLocal,
    Parser,
    ErrorMsg
) where

import qualified Lib1
import qualified Lib2
import qualified Lib3

import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Exception (SomeException, catch)
import Data.List (stripPrefix)

import Test.QuickCheck (Arbitrary(..), Gen, choose, elements)

-- ============================================================================
-- PARSER TYPES
-- ============================================================================

type ErrorMsg = String
type Input = String
type Parser = ExceptT ErrorMsg (State Input)

-- ============================================================================
-- LOW-LEVEL PARSER HELPERS
-- ============================================================================

consume :: String -> Parser ()
consume s = do
    input <- lift get
    case stripPrefix s input of
        Nothing   -> throwE $ "Expected: " ++ s
        Just rest -> lift (put rest)

consumeChar :: Char -> Parser ()
consumeChar c = do
    input <- lift get
    case input of
        (x:xs) | x == c -> lift (put xs)
        _ -> throwE $ "Expected character: " ++ [c]

quoted :: Parser String
quoted = do
    consumeChar '"'
    content <- go ""
    consumeChar '"'
    return content
  where
    go :: String -> Parser String
    go acc = do
        input <- lift get
        case input of
            []      -> throwE "Unterminated string"
            ('"':_) -> return acc
            (x:xs)  -> lift (put xs) >> go (acc ++ [x])

number :: Parser Int
number = do
    input <- lift get
    let (digits, rest) = span (`elem` ['0'..'9']) input
    if null digits
        then throwE "Expected number"
        else lift (put rest) >> return (read digits)

-- ============================================================================
-- COMMAND PARSER
-- ============================================================================

parseCommand :: Parser Lib1.Command
parseCommand =
        parseCreate
    <|> parseAddSong
    <|> parseAddPlaylist
    <|> parseShow
    <|> parseTotal
    <|> parsePlay
    <|> parseDump

parseCreate :: Parser Lib1.Command
parseCreate = do
    consume "create playlist "
    name <- quoted
    return (Lib1.CreatePlaylist name)

parseAddSong :: Parser Lib1.Command
parseAddSong = do
    consume "add song "
    title <- quoted
    consume " by "
    artist <- quoted
    consume " "
    dur <- number
    consume " to playlist "
    pl <- quoted
    return (Lib1.AddSong title artist dur pl)

parseAddPlaylist :: Parser Lib1.Command
parseAddPlaylist = do
    consume "add playlist "
    nested <- quoted
    consume " to playlist "
    parent <- quoted
    return (Lib1.AddPlaylist nested parent)

parseShow :: Parser Lib1.Command
parseShow = do
    consume "show playlist "
    name <- quoted
    return (Lib1.ShowPlaylist name)

parseTotal :: Parser Lib1.Command
parseTotal = do
    consume "total duration of playlist "
    name <- quoted
    return (Lib1.TotalDuration name)

parsePlay :: Parser Lib1.Command
parsePlay = do
    consume "play playlist "
    name <- quoted
    return (Lib1.PlayPlaylist name)

parseDump :: Parser Lib1.Command
parseDump = do
    consume "dump examples"
    return (Lib1.Dump Lib1.Examples)

-- ============================================================================
-- FREE MONAD DSL
-- ============================================================================

data PlaylistF next
    = CreatePlaylist String next
    | AddSong String String Int String next
    | AddPlaylist String String next
    | ShowPlaylist String (String -> next)
    | TotalDuration String (Int -> next)
    | PlayPlaylist String (String -> next)
    | DumpExamples ([String] -> next)
    deriving Functor

type PlaylistDSL = Free PlaylistF

-- Smart constructors

createPlaylist :: String -> PlaylistDSL ()
createPlaylist name = liftF (CreatePlaylist name ())

addSong :: String -> String -> Int -> String -> PlaylistDSL ()
addSong t a d p = liftF (AddSong t a d p ())

addPlaylist :: String -> String -> PlaylistDSL ()
addPlaylist n p = liftF (AddPlaylist n p ())

showPlaylist :: String -> PlaylistDSL String
showPlaylist n = liftF (ShowPlaylist n id)

totalDuration :: String -> PlaylistDSL Int
totalDuration n = liftF (TotalDuration n id)

playPlaylist :: String -> PlaylistDSL String
playPlaylist n = liftF (PlayPlaylist n id)

dumpExamples :: PlaylistDSL [String]
dumpExamples = liftF (DumpExamples id)

-- ============================================================================
-- HTTP INTERPRETER
-- ============================================================================

runHTTP :: PlaylistDSL a -> IO (Either String a)
runHTTP (Pure a) = return (Right a)

runHTTP (Free (CreatePlaylist n next)) =
    send (Lib2.toCliCommand (Lib1.CreatePlaylist n)) >>=
        either (return . Left) (const $ runHTTP next)

runHTTP (Free (AddSong t a d p next)) =
    send (Lib2.toCliCommand (Lib1.AddSong t a d p)) >>=
        either (return . Left) (const $ runHTTP next)

runHTTP (Free (AddPlaylist n p next)) =
    send (Lib2.toCliCommand (Lib1.AddPlaylist n p)) >>=
        either (return . Left) (const $ runHTTP next)

runHTTP (Free (ShowPlaylist n cont)) =
    send (Lib2.toCliCommand (Lib1.ShowPlaylist n)) >>=
        either (return . Left) (\r -> runHTTP (cont r))

runHTTP (Free (TotalDuration n cont)) =
    send (Lib2.toCliCommand (Lib1.TotalDuration n)) >>=
        either (return . Left) (\r -> runHTTP (cont (read r)))

runHTTP (Free (PlayPlaylist n cont)) =
    send (Lib2.toCliCommand (Lib1.PlayPlaylist n)) >>=
        either (return . Left) (\r -> runHTTP (cont r))

runHTTP (Free (DumpExamples cont)) =
    send "dump examples" >>=
        either (return . Left) (\r -> runHTTP (cont (lines r)))

send :: String -> IO (Either String String)
send cmd =
    catch
        (do
            req <- parseRequest "POST http://localhost:3000/command"
            let req' = setRequestBodyLBS (LBS.pack cmd) req
            resp <- httpBS req'
            return $ Right (BS.unpack (getResponseBody resp))
        )
        handler
  where
    handler :: SomeException -> IO (Either String String)
    handler e = return $ Left (show e)

-- ============================================================================
-- LOCAL INTERPRETER
-- ============================================================================

runLocal :: PlaylistDSL a -> Lib3.State -> IO (Either String (a, Lib3.State))
runLocal (Pure a) st = return (Right (a, st))

runLocal (Free op) st =
    case op of
        CreatePlaylist n next -> 
            stepSimple st (Lib1.CreatePlaylist n) next
        
        AddSong t a d p next  -> 
            stepSimple st (Lib1.AddSong t a d p) next
        
        AddPlaylist n p next  -> 
            stepSimple st (Lib1.AddPlaylist n p) next
        
        ShowPlaylist n cont   -> 
            stepWithResult st (Lib1.ShowPlaylist n) cont
        
        TotalDuration n cont  -> 
            stepWithResult st (Lib1.TotalDuration n) 
                (\output -> cont (read output :: Int))
        
        PlayPlaylist n cont   -> 
            stepWithResult st (Lib1.PlayPlaylist n) cont
        
        DumpExamples cont     ->
            let res = map Lib2.toCliCommand Lib1.examples
            in runLocal (cont res) st
  where
    -- For commands that don't return output (create, add, etc.)
    stepSimple :: Lib3.State -> Lib1.Command -> PlaylistDSL a 
               -> IO (Either String (a, Lib3.State))
    stepSimple currentSt cmd next =
        case Lib3.applyCommand cmd currentSt of
            Left err -> return (Left err)
            Right newSt -> runLocal next newSt
    
    -- For commands that return output (show, total, play)
    stepWithResult :: Lib3.State -> Lib1.Command -> (String -> PlaylistDSL a)
                   -> IO (Either String (a, Lib3.State))
    stepWithResult currentSt cmd cont =
        case Lib3.applyCommandR cmd currentSt of
            Left err -> return (Left err)
            Right (mout, newSt) ->
                let output = maybe "" id mout
                in runLocal (cont output) newSt

-- ============================================================================
-- QUICKCHECK INSTANCE
-- ============================================================================

instance Arbitrary Lib1.Command where
    arbitrary = do
        k <- choose (0,6) :: Gen Int
        case k of
            0 -> Lib1.CreatePlaylist <$> pname
            1 -> Lib1.AddSong <$> stitle <*> artist <*> choose (30,600) <*> pname
            2 -> Lib1.AddPlaylist <$> pname <*> pname
            3 -> Lib1.ShowPlaylist <$> pname
            4 -> Lib1.TotalDuration <$> pname
            5 -> Lib1.PlayPlaylist <$> pname
            _ -> return (Lib1.Dump Lib1.Examples)
      where
        pname  = elements ["Rock","Jazz","Pop","Metal","Chill"]
        stitle = elements ["Song A","Song B","Song C"]
        artist = elements ["Artist X","Artist Y","Artist Z"]