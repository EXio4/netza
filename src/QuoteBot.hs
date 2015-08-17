module QuoteBot (quoteBot,Config, K_Status(..), makeConfig, withConfig) where

import           IRC
import           IRC.Commands
import           System.IO
import           Data.Function
import           Data.Functor
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Read as T.R
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.ICU as Regex
import qualified Database.SQLite.Simple as DB
import           Database.SQLite.Simple (NamedParam((:=)))
import qualified System.Random as Rand

data Config (k :: K_Status) = Config {
     cfg_database    :: !(ConfigI k)
    ,cfg_prefix      :: !Text
    ,cfg_admins      :: !([Regex.Regex])
}

makeConfig :: String -> Text -> [Text] -> Config NotLoaded
makeConfig db prefix admins = Config (C_S db) prefix (map (Regex.regex []) admins)

withConfig :: Config NotLoaded -> (Config Loaded -> IO a) -> IO a
withConfig (Config (C_S cfg) prefix regex) cb = DB.withConnection cfg $ \db -> do
    DB.execute_ db "CREATE TABLE IF NOT EXISTS quotes ( quote_id INTEGER PRIMARY KEY, added_on INT, added_by TEXT, quote TEXT );"
    cb (Config (C_D db) prefix regex)

data K_Status = Loaded | NotLoaded

data ConfigI (k :: K_Status) where
    C_S :: String        -> ConfigI NotLoaded
    C_D :: DB.Connection -> ConfigI Loaded
 
data Loop = Loop | Quit
til :: Monad m => m Loop -> m ()
til k = fix $ \r -> k >>= \case Loop -> r
                                Quit -> return ()

database :: Config Loaded -> DB.Connection
database (Config (C_D x) _ _) = x

addQuote :: Config Loaded -> Channel -> Nick -> Text -> IRC IO ()
addQuote (database -> db) ch (Nick nick) quote = do
    liftIO (DB.executeNamed db
                "INSERT INTO quotes (added_on, added_by, quote) VALUES ( strftime('%s', 'now'), :nick, :quote );"
                [":nick"  := nick
                ,":quote" := quote
                ])
    privmsg ch . Message $ "Quote added! (by " <> nick <> ")"
    

rmQuote :: Config Loaded -> Channel -> Nick -> Integer -> IRC IO ()
rmQuote (database -> db) ch (Nick nick) quoteN = do
    liftIO $ DB.executeNamed db "DELETE FROM quotes WHERE quote_id=:qid" [":qid" := quoteN]
    privmsg ch . Message $ nick <> " removed the quote " <> T.pack (show quoteN)

randomQuote :: Config Loaded -> Channel -> IRC IO ()
randomQuote (database -> db) ch = do
    [DB.Only (cnt :: Integer)] <- liftIO (DB.query_ db "SELECT COUNT(*) AS cnt FROM quotes")
    n <- liftIO $ Rand.randomRIO (0, cnt-1)
    (xs :: [(Integer,Text)]) <- liftIO (DB.query db "SELECT quote_id, quote FROM quotes LIMIT ?, 1" (DB.Only n))
    privmsg ch . Message $ case xs of
         []            -> "No quotes in database"
         [(q_id, txt)] -> "#" <> T.pack (show q_id) <> " " <> txt
         xs            -> "ERR! " <> T.pack (show xs)

admin :: Config a -> Text -> Bool
admin (Config{cfg_admins=regexes}) xs = any (\x -> isJust (Regex.find x xs)) regexes

quoteBot :: Config Loaded -> IRC IO ()
quoteBot cfg = til loop where
    _cmd c = fmap (T.dropWhile (== ' ')) . T.stripPrefix (pref <> c)
    pref = cfg_prefix cfg
    loop = irc_read >>= \case
            CHMSG (User nick _ host) channel (Message msg)
                | Just xs <- _cmd "addquote" msg
                -> Loop <$ addQuote cfg channel nick xs
                | Just _  <- _cmd "quote" msg
                -> Loop <$ randomQuote cfg channel
                | Just xs <- _cmd "rmquote" msg
                , admin cfg host
                , Right (xid,_) <- T.R.decimal xs
                -> Loop <$ rmQuote cfg channel nick xid
                | Just _  <- _cmd "quit" msg
                , admin cfg host
                -> Quit <$ cmd "QUIT" ["bai"]
            _ -> return Loop
                


