module Hrypton (runServer) where
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

runServer = do
  hSetBuffering stdout NoBuffering
  runTCPServer Nothing "3000" $ talk Handshaking
  where
    talk :: PacketState -> Socket -> IO ()
    talk state s = do
      msg <- recv s 1024
      putStrLn $ "Received " ++ show msg
      let readPacket = runGet (getPacket state) msg
      res <- case readPacket of
        Left err -> fail err 
        Right packet -> handlePacket packet

      let nextState = case readPacket of
            Left err -> error err
            Right (Handshake _ _ _ nextStateInt) -> packetStateFrom nextStateInt
            Right _ -> state

      let byteResponse = case res of
            Just response -> runPut $ putPacket response
            Nothing -> S.empty
      print readPacket
      print res
      print nextState
      S.putStr byteResponse
      putStrLn ""
      unless (S.null byteResponse) $ sendAll s byteResponse
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