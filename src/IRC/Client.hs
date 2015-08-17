module IRC.Client (connectToIRC, connectToIRC'with, mutateIRC_cfg) where

import           Control.Applicative
import           Control.Monad
import           Data.Function
import           IRC.Types
import           IRC.Commands
import qualified IRC.Raw               as Raw
import           IRC.Raw.Types (IRC)
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Maybe
import           Control.Concurrent
import           Control.Concurrent.Chan
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T.E
import           Control.Monad.IO.Class

mutateIRC_cfg :: Monad m => IRCConfig -> IRC m a -> IRC m a
mutateIRC_cfg (IRCConfig network port (Nick nick) sasl channels) irc = pingPong $ do
        when (isJust sasl) $ do
            cmd "CAP" ["REQ", "sasl"]
        cmd "USER"  [nick, "x", "x", "hasky"]
        cmd "NICK"  [nick]
        authThen sasl channels irc

authThen ::  Monad m => Maybe SASLCfg -> [ChannelCfg] -> IRC m a -> IRC m a
authThen saslMay channels continue = loop
    where sasl_encode :: SASLCfg -> T.Text
          sasl_encode (SASLCfg username password) =
                T.E.decodeUtf8 . Base64.encode . T.E.encodeUtf8 . T.pack $
                        (username <> "\00" <> username <> "\00" <> password <> "\00")
          sasl_auth sasl x =
                case x of
                     (Raw.Message _ _ "CAP" (Raw.Params [_, "ACK", _ {- let's hope for SASL -} ])) -> Just $ do
                             cmd "AUTHENTICATE" ["PLAIN"]
                     (Raw.Message _ _ "AUTHENTICATE" (Raw.Params ["+"])) -> Just $ do
                             cmd "AUTHENTICATE" [sasl_encode sasl]
                     (Raw.Message _ _ "903" _) -> Just $ do
                             cmd "CAP" ["END"] {- logged in! -}
                     (Raw.Message _ _ "904" _) -> Just $ do
                             cmd "CAP" ["END"] {- auth failed -}
                     _ -> Nothing
          loop = do
            x <- Raw.irc_read
            let r = saslMay >>= (`sasl_auth` x)
            case x of
                _ | Just act <- r -> do
                        act
                        loop
                (Raw.Message _ _ (Raw.CmdNumber 376)  _) -> do
                        forM_ channels $ \(ChannelCfg (Channel ch) pwd) ->
                            cmd "JOIN" (ch : map T.pack (case pwd of
                                                            Nothing -> []
                                                            Just v  -> [v]))
                        continue -- we joined and that, so that's all
                _ -> loop


connectToIRC :: IRCConfig -> IRC IO a -> IO a
connectToIRC= connectToIRC'with id

connectToIRC'with :: MonadIO m => (forall a. m a -> IO a) -> IRCConfig -> IRC m a -> IO a
connectToIRC'with conv cfg@(IRCConfig network port nick sasl channels) irc = do
        Raw.connectToIRC_raw's network (fromIntegral port) $ \i ->
            conv $ Raw.runIRC (Raw.nirc_send i) (Raw.nirc_read i) (mutateIRC_cfg cfg irc)
        
pingPong :: Monad m => IRC m a -> IRC m a
pingPong = Raw.mutateIRC Raw.irc_send recv
    where recv = do
              x <- Raw.irc_read
              case x of
                    (Raw.Message _ _ "PING" params) -> do
                        Raw.irc_send (Raw.Message Nothing Nothing (Raw.Command "PONG") params)
                    _ -> return ()
              return x

