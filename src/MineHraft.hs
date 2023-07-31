module MineHraft (runServer) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import qualified Data.ByteString as S
import Data.Serialize
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Packets
import Server
import System.IO
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy  as BS

runServer = do
  BS.putStr $ A.encode x
  hSetBuffering stdout NoBuffering
  runTCPServer Nothing "3000" $ talk Handshaking
  where
    talk :: PacketState -> Socket -> IO ()
    talk state s = do
      msg <- recv s 1024
      putStrLn $ "Received " ++ show msg
      let readPacket = runGet (getPacket state) msg
      res <- either fail handlePacket readPacket

      let nextState = either error (incomingPacketState state) readPacket

      let byteResponse = maybe S.empty (runPut . putPacket) res

      unless (S.null byteResponse) $ do
        (sendAll s byteResponse) 
        putStrLn $ "Sent response " <> show byteResponse 
      talk nextState s

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      head <$> getAddrInfo (Just hints) mhost (Just port)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      setSocketOption sock NoDelay 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)