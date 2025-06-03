module WebsocketsAPI where

import Data.Text qualified as T
import Network.WebSockets qualified as WS

-- WebSocket Application
websocketsServerApp :: WS.ServerApp
websocketsServerApp pendingConn = do
    -- Accept the WebSocket connection
    conn <- WS.acceptRequest pendingConn
    -- We also fork a pinging thread in the background.
    -- This will ensure the connection stays alive on some browsers.
    WS.withPingThread conn 30 (return ()) $ do
        (msg :: T.Text) <- WS.receiveData conn
        print $ "First msg " <> msg

        -- Simulate a long task by counting to 100000
        let limit = 10000 :: Int
        forM_ [1 .. limit] $ \_ -> pure ()
        -- Once done, send a message to the client
        WS.sendTextData conn (T.pack $ "Done counting to " <> show limit)

        (msg2 :: T.Text) <- WS.receiveData conn
        print $ "Second msg " <> msg2