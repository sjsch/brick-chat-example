{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens hiding (view)
import Control.Monad (forever, unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GI.Gtk as G
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Network.Simple.TCP
import Pipes
import Shared
import System.Environment
import qualified GI.Gdk as Gdk
import Control.Concurrent.Async

data State = State {_chatNick :: Text, _chatSock :: Socket, _chatHistory :: [Message]}

makeLenses ''State

data Event = MessageEntered Text | MessageReceived Message | Closed

main :: IO ()
main = do
  [server, nick] <- getArgs
  (sock, _) <- connectSock server chatPort

  sendE sock (Join (T.pack nick))

  let receiver =
        forever $
          liftIO (recvE sock) >>= yield . MessageReceived
  
  void (G.init Nothing)
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  p <- G.cssProviderNew
  G.cssProviderLoadFromData p styles
  G.styleContextAddProviderForScreen
    screen
    p
    (fromIntegral G.STYLE_PROVIDER_PRIORITY_USER)

  void . async $ do
    void $ runLoop 
      App
        { view = view',
          update = update',
          inputs = [receiver],
          initialState = State (T.pack nick) sock []
        }
    closeSock sock
    G.mainQuit

  G.main

  where
    styles = mconcat
      [ ".nick { font-weight: bold; min-width: 5em; margin-right: 5px; }",
        ".server { font-style: italic; }"
      ]

view' :: State -> AppView G.Window Event
view' s =
  bin
    G.Window
    [ #title := "chat",
      on #deleteEvent (const (True, Closed)),
      #widthRequest := 400,
      #heightRequest := 300
    ]
    $ container
      G.Box
      [#orientation := G.OrientationVertical]
      [ BoxChild
          (defaultBoxChildProperties {padding = 5})
          ( container
              G.Box
              []
              [ BoxChild
                  (defaultBoxChildProperties {padding = 5})
                  (widget G.Label [#label := (s ^. chatNick)]),
                BoxChild
                  (defaultBoxChildProperties {expand = True, fill = True, padding = 5})
                  ( widget
                      G.Entry
                      [ onM #activate \e -> do
                          t <- G.entryGetText e
                          G.entrySetText e ""
                          pure (MessageEntered t)
                      ]
                  )
              ]
          ),
        BoxChild
          (defaultBoxChildProperties {expand = True, fill = True})
          (historyView (s ^. chatHistory))
      ]

historyView :: [Message] -> Widget Event
historyView hist =
  bin
    G.ScrolledWindow
    []
    ( container
        G.Box
        [#orientation := G.OrientationVertical, #spacing := 3]
        (V.fromList (map historyItem hist))
    )
  where
    historyItem msg =
      container
        G.Box
        []
        [ widget
            G.Label
            [ #label := _nick msg,
              #xalign := 1,
              classes ["nick"]
            ],
          BoxChild
            (defaultBoxChildProperties {fill = True})
            (historyWidget msg)
        ]

    historyWidget (Message _ msg) =
      widget
        G.Label
        [ #label := msg,
          #wrap := True
        ]
    historyWidget (Join _) = widget G.Label [#label := "joined", classes ["server"] ]
    historyWidget (Leave _) = widget G.Label [#label := "left", classes ["server"] ]

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit
update' s (MessageEntered msg) = Transition s do
  unless (T.null msg) $
    sendE (s ^. chatSock) (Message (s ^. chatNick) msg)
  pure Nothing
update' s (MessageReceived msg) =
  Transition
    (s & chatHistory %~ cons msg)
    (pure Nothing)
