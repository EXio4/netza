{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IRC.Raw.Types where

import           Data.ByteString (ByteString) 
import           Control.Concurrent.Chan (Chan)
import           Control.Monad.Trans.Free
import           Data.String
import           Text.Read
{- naive representation of the IRC BNF -}

data Message = Message (Maybe Tags) (Maybe Prefix) Command Params
    deriving (Show,Eq)

data Tags    = Tags [Tag]
    deriving (Show,Eq)

data Prefix  = ServerName Host
             | Prefix Nick (Maybe User) (Maybe HostP)
    deriving (Show,Eq)

newtype Nick    = Nick ByteString
    deriving (Show,Eq,IsString)

newtype User    = User ByteString
    deriving (Show,Eq,IsString)

data HostP = ValidHost   Host
           | InvalidHost ByteString
    deriving (Show,Eq)

newtype Host    = Host ByteString
    deriving (Show,Eq,IsString)


data Command = CmdNumber Int
             | Command ByteString
    deriving (Show,Eq)

instance IsString Command where
    fromString x = case readMaybe x of
                        Nothing -> Command (fromString x)
                        Just i  -> CmdNumber i

data Params  = Params [Param]
    deriving (Show,Eq)

newtype Param   = Param ByteString
    deriving (Show,Eq,IsString)

data Tag     = Tag Key (Maybe ByteString)
    deriving (Show,Eq)

data Key     = Key (Maybe Vendor) ByteString
    deriving (Show,Eq)

newtype Vendor  = Vendor Host
    deriving (Show,Eq,IsString)


data IRC_Connection
    = IRC_Connection
        (Chan Message) -- ^ input  stream (IRC -> HS)
        (Chan Message) -- ^ output stream (HS -> IRC)

data IRCF a 
        = IRC_Read (Message -> a)
        | IRC_Send Message a

type IRC = FreeT IRCF
