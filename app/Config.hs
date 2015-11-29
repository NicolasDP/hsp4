{-# LANGUAGE OverloadedStrings #-}

module Config
    ( P4Config(..)
    , loadConfig
    , expectOption
    , p4ConfigToList
    , defaultConfig
    ) where

-- import System.Environment

import System.Directory
import System.FilePath
import Control.Applicative ((<$>), (<|>))
import Data.String.Parse (Parser, Result(..))
import qualified Data.String.Parse as Parser

data P4Config = P4Config
    { user     :: String
    , port     :: String
    , client   :: Maybe String
    , password :: Maybe String
    } deriving (Eq, Show, Read)

expectOption :: String -> (P4Config -> Maybe a) -> P4Config -> IO ()
expectOption str getter cfg = case getter cfg of
    Nothing -> error $ "Missing configuration value: " ++ str ++ "\n\nsee 'hsp4 init'"
    Just _  -> return ()

maybeAdd :: String
         -> (Maybe String)
         -> [(String, String)]
         -> [(String, String)]
maybeAdd _ Nothing l = l
maybeAdd n (Just a) l = (n, a):l

p4ConfigToList :: P4Config -> [(String, String)]
p4ConfigToList cfg =
    maybeAdd "P4CLIENT" (client cfg) $
    maybeAdd "P4PASS"   (password cfg) $
          ("P4PORT", port cfg)
        : ("P4USER", user cfg)
        : []

p4Path :: FilePath
p4Path = ".p4config"

defaultConfig :: String
defaultConfig =
    "# HSP4 Default Configuration file\n"
    ++ "#\n"
    ++ "# put this configuration file into your home directory ${HOME}/.p4config\n"
    ++ "# You can put the default credential information to use (p4port and user)\n"
    ++ "#\n"
    ++ "# Then, you can add the other information into your /Path/to/project/.p4config\n"
    ++ "# Every new value you will put in this new file will override the value from\n"
    ++ "# the one in the home directory.\n"
    ++ "#\n"
    ++ "# Set the perforce instance you want to access\n"
    ++ "p4port: \"perfoce.address:1666\"\n"
    ++ "\n"
    ++ "# Set your perforce remote login access\n"
    ++ "user: \"perforce-login\"\n"
    ++ "\n"
    ++ "# Set your perforce remote password access\n"
    ++ "# If you set this value, you won't have to login every time.\n"
    ++ "# This value is optional (and not recommanded).\n"
    ++ "# Prefere using hsp4 login command instead.\n"
    ++ "# password: \"perforce-password\"\n"
    ++ "\n"
    ++ "# Set your perforce client workspace name\n"
    ++ "# set this value in your local hsp4 config\n"
    ++ "# client: \"perforce-client\"\n"

loadConfig :: IO P4Config
loadConfig = do
    egCfg <- parseConfigFileHome
    ecCfg <- parseConfigFileLocal

    case (egCfg, ecCfg) of
        (Left err, _       ) -> error err
        (_       , Left err) -> error err
        (Right l1, Right l2) -> 
            case buildConfig (l2 ++ l1) of
                Left err -> error err
                Right v  -> return v

  where
    buildConfig :: [(String, String)] -> Either String P4Config
    buildConfig l = do
        u <- mandatory "user" l
        p <- mandatory "p4port" l
        let c    = lookup "client" l
        let pass = lookup "password" l
        Right $ P4Config u p c pass
    
    mandatory :: String -> [(String, String)] -> Either String String
    mandatory k db = case lookup k db of
        Nothing -> Left $ "missing entry: '" ++ k ++ "' in config file"
        Just v  -> Right v

parseConfigFileHome :: IO (Either String [(String, String)])
parseConfigFileHome = getHomeDirectory >>= parseConfigFile

parseConfigFileLocal :: IO (Either String [(String, String)])
parseConfigFileLocal = getCurrentDirectory >>= parseConfigFileDir

parseConfigFileDir :: FilePath -> IO (Either String [(String, String)])
parseConfigFileDir dir = do
    isExist <- doesFileExist $ dir </> p4Path
    homeDir <- getHomeDirectory
    if isExist
        then parseConfigFile dir
        else do
            -- check that the parent directory is not the root directory
            -- nor the home directory
            let parentDir = takeDirectory dir
            if parentDir == homeDir || parentDir == dir
                then return $ Right []
                else parseConfigFileDir parentDir

parseConfigFile :: FilePath -> IO (Either String [(String, String)])
parseConfigFile projectPath = do
    isExist <- doesFileExist configPath
    if isExist
        then do
            content <- readFile configPath
            return $ case Parser.parse parseKeyValues content of
                ParseOK _ cfg -> Right cfg
                ParseFail err -> Left $ "error while parsing configuration file " ++ show configPath ++ ": " ++ err
                ParseMore _   -> Left $ "error while parsing configuration file " ++ show configPath ++ ": Not enough data"
        else return $ Right []
  where
    configPath :: FilePath
    configPath = projectPath </> p4Path

parseKeyValues :: Parser [(String, String)]
parseKeyValues = do
    dropEmptyLinesOrComments <|> return ()
    isEnd <- Parser.isEndOfBuff
    if isEnd
        then return []
        else do
            x  <- parseKeyValue
            xs <- parseKeyValues
            return $ x:xs

many :: Parser a -> Parser [a]
many p = do
    isEnd <- Parser.isEndOfBuff
    if isEnd
        then return []
        else do
            v <- p
            manyAgain v <|> return [v]
  where
    manyAgain v = do
        vs <- many p
        return $ v:vs

dropEmptyLinesOrComments :: Parser ()
dropEmptyLinesOrComments = do
    _ <- many $ (dropComment >> Parser.char '\n') <|> (dropSpaces >> Parser.char '\n')
    return ()

dropSpaces :: Parser ()
dropSpaces = do
    Parser.skipWhile (flip elem [' ', '\t'])

dropComment :: Parser ()
dropComment = do
    dropSpaces
    Parser.char '#'
    Parser.skipWhile ((/=) '\n')

parseKeyValue :: Parser (String, String)
parseKeyValue = do
    dropSpaces
    key <- parseKey
    dropSpaces
    Parser.char ':'
    dropSpaces
    value <- parseValue
    dropSpaces
    dropComment <|> return ()
    Parser.char '\n'
    return (key, value)

parseKey :: Parser String
parseKey =
    Parser.takeWhile (flip elem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

parseValue :: Parser String
parseValue =
    parseQuotedValue <|> parseValueStd

parseValueStd :: Parser String
parseValueStd =
    Parser.takeWhile (flip elem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.,;!?@"))

parseQuotedValue :: Parser String
parseQuotedValue = do
    Parser.char '"'
    str <- Parser.takeWhile ((/=) '"')
    Parser.char '"'
    return str
