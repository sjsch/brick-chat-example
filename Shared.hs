{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Shared where

import Control.Exception
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics
import Network.Simple.TCP

chatPort :: String
chatPort = "3333"

instance Serialize Text where
  put = put . TE.encodeUtf8
  get = fmap TE.decodeUtf8 get

data Message
  = Join {_nick :: Text}
  | Leave {_nick :: Text}
  | Message {_nick :: Text, _content :: Text}
  deriving (Show, Generic, Serialize)

sendE :: Socket -> Message -> IO ()
sendE s = send s . encode

recvE :: Socket -> IO Message
recvE s = do
  msg <- recv s 4096
  msg' <- maybe (throwIO DisconnectException) pure msg
  let Right msg'' = decode msg'
  pure msg''

data DisconnectException = DisconnectException
  deriving (Show)

instance Exception DisconnectException
