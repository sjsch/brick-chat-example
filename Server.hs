{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Network.Simple.TCP
import Shared

main = do
  msgC <- newChan
  serve HostIPv4 chatPort (handleConn msgC)

handleConn msgC (sock, _) = do
  Join nick <- recvE sock
  writeChan msgC (Join nick)

  myC <- dupChan msgC
  let sendLoop = handle (\(_ :: IOException) -> pure ()) $ do
        msg <- readChan myC
        sendE sock msg
        sendLoop
  forkIO sendLoop

  let cleanup (_ :: DisconnectException) = writeChan msgC (Leave nick)
  handle cleanup $ recvLoop nick
  where
    recvLoop nick = do
      Message _ msg <- recvE sock
      writeChan msgC (Message nick msg)
      recvLoop nick
