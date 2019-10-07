module WebSocket.Extra where

import Prelude (class Show, show, (<>), (<<<), discard)
import WebSocket (WebSocketsApp (..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)


logConsole :: forall m send receive
            . MonadEffect m
           => Show send
           => Show receive
           => WebSocketsApp m receive send
           -> WebSocketsApp m receive send
logConsole (WebSocketsApp f) = WebSocketsApp \env ->
  let {onopen,onmessage,onclose,onerror} = f env
  in  { onopen: onopen <<< newParams
      , onmessage: \c x -> do
          liftEffect (log ("Received: " <> show x))
          onmessage (newParams c) x
      , onclose
      , onerror
      }
  where
    newParams c@{send} =
      c { send = \x -> do
            liftEffect (log ("Sent: " <> show x))
            send x
        }
