{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module IRC.Types  where

import           Control.Lens
import           Data.Set  (Set)
import           Data.Map  (Map)
import qualified IRC.Raw.Types as Raw
import           IRC.Raw.Monad
import           Data.Monoid
import           Data.ByteString (ByteString)
import           Data.Text (Text)

data Cmd = N Int
         | S ByteString
  deriving (Show,Eq,Ord)  -- used by IRC.Commands

data User = User Nick Ident Host 
    deriving (Show,Eq)
    
userNick :: User -> Nick
userNick (User n _ _) = n
        
newtype Channel = Channel Text
    deriving (Show,Eq)
newtype Nick    = Nick    Text
    deriving (Show,Eq,Ord)
newtype Message = Message Text
    deriving (Show,Eq,Monoid)
newtype Target  = Target Text
    deriving (Show,Eq)
newtype Account = Account Text
    deriving (Show,Eq,Ord)
type Ident   = Text
type Host    = Text

data Mode = Plus  Char
          | Minus Char
    deriving (Show,Eq)
data CMode = CMode Nick Mode
    deriving (Show,Eq)
    
data SASLCfg = SASLCfg {
     sasl_username :: String
    ,sasl_password :: String
} deriving (Show,Eq)

data ChannelCfg = ChannelCfg {
     channel_name     :: Channel
    ,channel_password :: Maybe String
} deriving (Show,Eq)
    
data IRCConfig = IRCConfig {
     config_network  :: String
    ,config_port     :: Int
    ,config_nick     :: Nick
    ,config_sasl     :: Maybe SASLCfg
    ,config_channels :: [ChannelCfg]
} deriving (Show,Eq)

