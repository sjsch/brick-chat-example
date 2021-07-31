{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Zipper
import Graphics.Vty
import Lens.Micro
import Lens.Micro.TH
import Network.Simple.TCP
import Shared
import System.Environment
import System.IO

data Name = NameEditor
  deriving (Eq, Ord, Show)

data State = State
  { _chatEditor :: Editor Text Name,
    _chatNick :: Text,
    _chatSock :: Socket,
    _chatHistory :: [Message]
  }

makeLenses ''State

main = do
  [server, nick] <- getArgs
  (sock, _) <- connectSock server chatPort

  sendE sock (Join (T.pack nick))

  let startState =
        State
          { _chatEditor = editorText NameEditor (Just 1) "",
            _chatNick = T.pack nick,
            _chatSock = sock,
            _chatHistory = []
          }
      buildVty = mkVty mempty
  vty <- buildVty

  eventC <- newBChan 5
  let recvLoop = handle (\(_ :: IOException) -> pure ()) $
        forever $ recvE sock >>= writeBChan eventC
  forkIO recvLoop

  customMain vty buildVty (Just eventC) app startState
  closeSock sock
  where
    app =
      App
        { appDraw = draw,
          appChooseCursor = showFirstCursor,
          appHandleEvent = handleEvent,
          appStartEvent = pure,
          appAttrMap = \_ ->
            attrMap
              defAttr
              [ ("nick", defAttr `withStyle` bold),
                ("server", defAttr `withForeColor` green)
              ]
        }

handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s
handleEvent s (VtyEvent (EvKey KEnter [])) = do
  let (msg', s') = s & chatEditor . editContentsL <<%~ clearZipper
      msg = currentLine msg'
  unless (T.null msg) $
    liftIO $ sendE (s ^. chatSock) (Message (s ^. chatNick) msg)
  continue s'
handleEvent s (VtyEvent ev) = do
  s <- handleEventLensed s chatEditor handleEditorEvent ev
  continue s
handleEvent s (AppEvent msg) = continue $ s & chatHistory %~ (msg :)
handleEvent s _ = continue s

draw s =
  [ padBottom Max $
      vBox
        [ drawNick (s ^. chatNick)
            <+> renderEditor (hBox . map txt) True (s ^. chatEditor),
          hBorder,
          historyWidget
        ]
  ]
  where
    historyWidget = Widget Greedy Greedy renderHistory

    renderHistory = do
      ctx <- getContext
      let height = ctx ^. availHeightL
      let widget = vBox . map drawMessage . take height $ s ^. chatHistory
      render widget

    drawNick nick =
      hLimit
        10
        (padLeft Max (withAttr "nick" (txt ("<" <> nick <> "> "))))

    drawMessage msg =
      drawNick (_nick msg)
        <+> case msg of
          Join _ -> withAttr "server" $ txt "joined"
          Leave _ -> withAttr "server" $ txt "left"
          Message _ m -> txtWrap m
