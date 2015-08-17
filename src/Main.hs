{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import           Data.Set (Set)
import qualified IRC.Client as IRC
import           IRC
import           Control.Monad
import           Control.Applicative
import           Data.Functor
import           Data.Monoid
import           Control.Concurrent
import           System.Environment
import           System.IO
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Aeson
import           Options.Applicative
import qualified Data.Yaml as YAML
import           Control.Exception
import           QuoteBot


data CFG = CFG {
         cfg_irc :: IRCConfig
        ,cfg_bot :: Config NotLoaded
}

instance FromJSON (Config NotLoaded) where
    parseJSON (Object v) = makeConfig <$> v .: "database" <*> v .: "prefix" <*> v .: "admins"
    parseJSON _ = fail "bot_config"

instance FromJSON CFG where
    parseJSON (Object v) = CFG <$> v .: "irc" <*> v .: "bot"
    parseJSON _ = fail "main_cfg"
            

data Arguments = Args {
    args_config :: FilePath
}

args :: Parser Arguments
args = Args <$> strOption (  long "config"
                          <> short 'c'
                          <> metavar "CONFIG"
                          <> help "Configuration file of the bot, documented in config.yaml.example")


prog :: CFG -> IO ()
prog (CFG irc_config bot_config) =
    withConfig bot_config $ \bot_config' -> 
                    IRC.connectToIRC irc_config (quoteBot bot_config')

main :: IO ()
main = do
    let opts = info (helper <*> args)
                (fullDesc
                <> header   "netza - quote bot")
    Args cfg <- execParser opts        
    x <- try (YAML.decodeFileEither cfg)
    case x of
            Left (x :: IOError) -> do
                putStrLn "error reading config file"
                print x
            Right (Left x) -> do
                putStrLn "error parsing config file"
                print x
            Right (Right cfg) -> prog cfg

    

        