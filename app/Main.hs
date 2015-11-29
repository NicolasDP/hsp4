module Main (main) where

import System.Environment
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad (when)
import System.Process
import System.Exit

import Config

tryCommand_ :: P4Config
            -> String
            -> [String]
            -> IO (Maybe String)
tryCommand_ cfg cmd opt = do
    e <- getEnvironment -- retrieve all the environment variables
    ph <- runProcess
            cmd
            opt
            Nothing -- CWD
            (Just (e ++ p4ConfigToList cfg))
            Nothing -- stdin
            Nothing -- stdout
            Nothing -- stderr
    e <- waitForProcess ph
    return $ case e of
        ExitSuccess   -> Nothing
        ExitFailure i -> Just $ "\"" ++ cmd ++ " " ++ (show opt) ++"\" exit with " ++ show i

userLoggedIn :: P4Config -> IO Bool
userLoggedIn cfg = do
    mLoggedIn <- tryCommand_ cfg "p4" ["login", "-s"]
    return $ case mLoggedIn of
        Nothing -> True
        Just _  -> False

withLoggedIn :: P4Config -> IO a -> IO a
withLoggedIn cfg action = do
    condition <- userLoggedIn cfg
    when (not condition) $ tryP4Connect cfg
    condition' <- userLoggedIn cfg
    if condition'
        then action
        else error "cannot connect to the perforce server. Check your connection and configuration files"

tryCommand :: P4Config
           -> String
           -> [String]
           -> IO ()
tryCommand cfg cmd opt = do
    e <- tryCommand_ cfg cmd opt
    case e of
        Nothing  -> return ()
        Just err -> error err

tryClone :: [String] -> P4Config -> IO ()
tryClone opts cfg = withLoggedIn cfg $
    tryCommand cfg "git" ("p4" : "clone" : opts)

tryRebase :: P4Config -> IO ()
tryRebase cfg = withLoggedIn cfg $
    tryCommand cfg "git" ("p4" : "rebase" : [])

trySync :: P4Config -> IO ()
trySync cfg = withLoggedIn cfg $
    tryCommand cfg "git" ("p4" : "sync" : [])

trySubmit :: P4Config -> IO ()
trySubmit cfg = withLoggedIn cfg $ do
    expectOption "client" Config.client cfg
    tryCommand cfg "git" ("p4" : "submit" : [])

tryP4Connect :: P4Config -> IO ()
tryP4Connect cfg = tryCommand cfg "p4" ("login" : [])

tryP4Disconnect :: P4Config -> IO ()
tryP4Disconnect cfg = tryCommand cfg "p4" ("logout" : [])

tryP4Client :: P4Config -> IO ()
tryP4Client cfg = withLoggedIn cfg $ tryCommand cfg "p4" ("client" : [])

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["init"] -> putStrLn defaultConfig
        "clone":opts | length opts > 0 -> tryClone opts =<< loadConfig
                     | otherwise       -> error "Error: expect \"clone\" P4Path [dir]"
        ["client"] -> tryP4Client =<< loadConfig
        ["login"]  -> tryP4Connect =<< loadConfig
        ["logout"]  -> tryP4Disconnect =<< loadConfig
        ["rebase"] -> tryRebase =<< loadConfig
        ["submit"] -> trySubmit =<< loadConfig
        ["sync"]   -> trySync =<< loadConfig
        _ -> error $ "argument failed: " ++ show args
