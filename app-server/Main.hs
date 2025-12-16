{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty
import qualified Lib1
import qualified Lib2
import qualified Lib3
import qualified Lib4
import Control.Concurrent (Chan, newChan, forkIO)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
import Control.Monad.Trans.State.Strict (evalState)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.HTTP.Types.Status (status400)
import System.IO (hFlush, stdout)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    putStrLn "=== Playlist Manager Server ==="
    putStrLn "Starting server on http://localhost:3000"
    
    -- Initialize state
    stateVar <- newTVarIO Lib3.emptyState
    
    -- Create storage channel and start storage loop
    storageChan <- newChan
    _ <- forkIO $ Lib3.storageOpLoop storageChan
    
    -- Load saved state
    putStrLn "Loading saved state..."
    loadResult <- Lib3.load storageChan stateVar
    case loadResult of
        Left err -> putStrLn $ "Warning: Could not load state: " ++ err
        Right () -> putStrLn "State loaded successfully"
    
    putStrLn "Server ready! Listening for commands..."
    putStrLn "Press Ctrl+C to stop"
    hFlush stdout
    
    -- Start HTTP server
    scotty 3000 $ do
        -- Logging middleware
        middleware logStdoutDev
        
        -- Health check
        get "/" $ text "Playlist Manager Server - OK"
        
        -- Command endpoint
        post "/command" $ do
            bodyBytes <- body
            let commandStr = TL.unpack $ TLE.decodeUtf8 bodyBytes
                parseResult = evalState (runExceptT Lib4.parseCommand) commandStr
            case parseResult of
                Left err -> do
                    status status400
                    text $ TL.pack $ "Parse error: " ++ err
                Right cmd -> do
                    result <- liftIO $ executeCommand stateVar cmd
                    -- Auto-save for mutating commands
                    case cmd of
                        Lib1.Dump _ -> return ()
                        Lib1.ShowPlaylist _ -> return ()
                        Lib1.TotalDuration _ -> return ()
                        Lib1.PlayPlaylist _ -> return ()
                        _ -> do
                            _ <- liftIO $ Lib3.save storageChan stateVar
                            return ()
                    text $ TL.pack result
        
        -- Shutdown endpoint
        post "/shutdown" $ do
            liftIO $ do
                putStrLn "\nShutdown requested, saving state..."
                saveResult <- Lib3.save storageChan stateVar
                case saveResult of
                    Left err -> putStrLn $ "Error saving: " ++ err
                    Right () -> putStrLn "State saved successfully"
            text "Server shutting down..."

-- Execute command and return result string
executeCommand :: TVar Lib3.State -> Lib1.Command -> IO String
executeCommand stateVar cmd = do
    result <- atomically $ do
        currentState <- readTVar stateVar
        case Lib3.applyCommandR cmd currentState of
            Left err -> return $ Left err
            Right (mout, newState) -> do
                writeTVar stateVar newState
                return $ Right mout
    case result of
        Left err -> return $ "Error: " ++ err
        Right mout -> 
            case mout of
                Nothing -> return "Command executed successfully"
                Just output -> return output