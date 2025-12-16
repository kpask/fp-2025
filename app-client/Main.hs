{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Lib1
import qualified Lib2
import qualified Lib3
import qualified Lib4
import Control.Concurrent (Chan, newChan, forkIO)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
import Control.Monad.Trans.State.Strict (evalState, runState)
import Control.Monad.Trans.Except (runExceptT)
import System.IO (hFlush, stdout)
import Data.Char (isSpace)

main :: IO ()
main = do
    putStrLn "=== Playlist Manager Client ==="
    putStrLn "Type 'help' for available commands"
    putStrLn "Type 'quit' to exit"
    putStrLn ""
    
    -- Initialize state
    stateVar <- newTVarIO Lib3.emptyState
    
    -- Create storage channel and start storage loop
    storageChan <- newChan
    _ <- forkIO $ Lib3.storageOpLoop storageChan
    
    -- Load saved state
    loadResult <- Lib3.load storageChan stateVar
    case loadResult of
        Left err -> putStrLn $ "Warning: Could not load state: " ++ err
        Right () -> putStrLn "State loaded successfully\n"
    
    -- Main REPL loop
    repl stateVar storageChan

repl :: TVar Lib3.State -> Chan Lib3.StorageOp -> IO ()
repl stateVar storageChan = do
    putStr "> "
    hFlush stdout
    input <- getLine
    let trimmed = trim input
    
    case trimmed of
        "" -> repl stateVar storageChan
        "quit" -> do
            putStrLn "Saving state..."
            saveResult <- Lib3.save storageChan stateVar
            case saveResult of
                Left err -> putStrLn $ "Error saving: " ++ err
                Right () -> putStrLn "State saved. Goodbye!"
        "help" -> do
            printHelp
            repl stateVar storageChan
        "save" -> do
            saveResult <- Lib3.save storageChan stateVar
            case saveResult of
                Left err -> putStrLn $ "Error: " ++ err
                Right () -> putStrLn "State saved successfully"
            repl stateVar storageChan
        "load" -> do
            loadResult <- Lib3.load storageChan stateVar
            case loadResult of
                Left err -> putStrLn $ "Error: " ++ err
                Right () -> putStrLn "State loaded successfully"
            repl stateVar storageChan
        _ -> do
            -- Try to parse the command using Lib4 parser
            let parseResult = evalState (runExceptT Lib4.parseCommand) trimmed
            case parseResult of
                Left err -> putStrLn $ "Parse error: " ++ err
                Right cmd -> do
                    -- Execute the command via Lib3
                    result <- atomically $ do
                        st <- readTVar stateVar
                        case Lib3.applyCommandR cmd st of
                            Left err -> return (Left err)
                            Right (mout, newSt) -> do
                                writeTVar stateVar newSt
                                return (Right mout)
                    case result of
                        Left err -> putStrLn $ "Error: " ++ err
                        Right mout ->
                            case mout of
                                Nothing -> putStrLn "Command executed successfully"
                                Just output -> putStrLn output
            repl stateVar storageChan

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

printHelp :: IO ()
printHelp = do
    putStrLn "Available commands:"
    putStrLn "  create playlist \"name\"                     - Create a new playlist"
    putStrLn "  add song \"title\" by \"artist\" <duration> to playlist \"name\""
    putStrLn "                                              - Add a song to a playlist"
    putStrLn "  add playlist \"nested\" to playlist \"parent\" - Add a playlist to another"
    putStrLn "  show playlist \"name\"                       - Display playlist contents"
    putStrLn "  total duration of playlist \"name\"          - Show total duration"
    putStrLn "  play playlist \"name\"                       - Play a playlist"
    putStrLn "  dump examples                               - Show example commands"
    putStrLn "  save                                        - Save current state"
    putStrLn "  load                                        - Load saved state"
    putStrLn "  help                                        - Show this help"
    putStrLn "  quit                                        - Exit the program"
    putStrLn ""