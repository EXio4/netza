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
import           QuoteBot

cfg net p nick ch = IRCConfig net p (Nick (T.pack nick)) Nothing [ChannelCfg (Channel (T.pack ch)) Nothing]
            
            
config :: Config NotLoaded
config = makeConfig "quotes.db"
                ["^unaffiliated/hacker$"]

main :: IO ()
main = do
    x <- getArgs
    case x of
         [network  , port  , nick, ch] -> do
                 let irc_config = cfg network (read port) nick ch
                 withConfig config $ \config' -> 
                    IRC.connectToIRC irc_config (quoteBot config' "!")
         xs -> putStrLn "invalid params"
         
    

        