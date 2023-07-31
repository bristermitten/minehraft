module MineHraft.MineHraft where

import Data.ByteString.Lazy qualified as BS
import MineHraft.Binary.Read (ReadBinary, runReadBinaryFromSocket)
import MineHraft.Binary.Write (execWriteBinary)
import MineHraft.Packet.Binary (getPacket, putPacket)
import MineHraft.PacketHandler (handlePacket)
import Polysemy (Embed, Member, Members, Sem, embed, raise, raise_, run, runM)
import Polysemy.State

import Control.Lens ((^.))
import MineHraft.Packet.Types (GenericOutgoingPacket)
import MineHraft.Session (HasSession (sessionState), Session (..))
import MineHraft.Session.Store (SessionStore, getOrCreateSession, putSession)
import Network.Socket (AddrInfo (..), AddrInfoFlag (AI_PASSIVE), HostName, ServiceName, SocketType (Stream), defaultHints, getAddrInfo, withSocketsDo)
import Polysemy.Error (Error, errorToIOFinal, runError)
import Polysemy.Resource (Resource, bracket, resourceToIOFinal, runResource)
import Polysemy.Socket (HandleSocket, ReadSocket, SemSocket, SocketInfo, WriteSocket, close, getPeerName, open, runHandleSocket, runRead, runSocketInfo, runWrite, write)
import Prelude hiding (State, evalState, execState, runState, trace)

process :: Member ReadBinary r => Session -> Sem r (Session, Maybe GenericOutgoingPacket)
process session = do
    packet <- getPacket False (session ^. sessionState)
    let (newSession, nextPacket) = run $! runState @Session session (handlePacket packet)
    pure (newSession, nextPacket)

respond :: Members '[State SessionStore, WriteSocket] r => (Session, Maybe GenericOutgoingPacket) -> Sem r ()
respond (newSession, nextPacket) = do
    putSession newSession
    whenJust nextPacket $ \next -> do
        let toWrite = run $ execWriteBinary (putPacket False next)
        unless (BS.null toWrite) $ write (fromLazy toWrite)

talk :: Members '[State SessionStore, Error Text, ReadSocket, WriteSocket, SocketInfo] r => Sem r ()
talk = do
    socketName <- getPeerName
    session <- getOrCreateSession socketName
    next <- runReadBinaryFromSocket $ process session
    respond next

runServer :: IO ()
runServer = do
    hSetBuffering stdout NoBuffering

    let y = runTCPServer Nothing "8080" talk :: Sem '[State SessionStore, Error Text, Embed IO] ()
    z <- y & evalState @SessionStore mempty & runError & runM
    case z of
        Left err -> putTextLn $ "Error: " <> err
        Right _ -> pass

runTCPServer :: Members '[Embed IO] r => Maybe HostName -> ServiceName -> Sem (ReadSocket ': WriteSocket ': SocketInfo ': r) a -> Sem r a
runTCPServer maybeHost port server = runHandleSocket $ do
    addr <- embed resolve
    let open' = raise $ open addr
    let close' = close
    let server' s = raise_ $ runSocketInfo s $ runWrite s $ runRead s server
    runResource $ bracket open' close' server'
  where
    resolve :: IO AddrInfo
    resolve = do
        let hints =
                defaultHints
                    { addrFlags = [AI_PASSIVE]
                    , addrSocketType = Stream
                    }
        a : _ <- getAddrInfo (Just hints) maybeHost (Just port)
        pure a
