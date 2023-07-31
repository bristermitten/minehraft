{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Socket where

import Control.Concurrent (forkFinally)
import Control.Exception (bracketOnError)
import Data.ByteString qualified as BS
import Network.Socket (AddrInfo (addrAddress), SockAddr, Socket, SocketOption (ReuseAddr), accept, bind, gracefulClose, listen, openSocket, setCloseOnExecIfNeeded, setSocketOption, withFdSocket)
import Network.Socket qualified as S
import Network.Socket.ByteString
import Polysemy (Member, Members, Sem, interpret, makeSem, pureT, reinterpretH, runM, run)
import Polysemy.Embed
import Polysemy.Final (interpretFinal)
import Prelude hiding (State, get, put)

newtype SemSocket = SemSocket {unSemSocket :: Socket} deriving (Show, Eq)

data ReadSocket m a where
    NextByte :: ReadSocket m (Maybe Word8)
    NextN :: Int -> ReadSocket m (Maybe ByteString)

makeSem ''ReadSocket

runRead :: Member (Embed IO) r => SemSocket -> Sem (ReadSocket ': r) a -> Sem r a
runRead (SemSocket socket) = interpret $ \case
    NextByte -> embed $ do
        bytes <- recv socket 1
        pure $ if BS.null bytes then Nothing else Just (BS.head bytes)
    NextN n -> embed $ do
        bytes <- recv socket n
        pure $ if BS.null bytes then Nothing else Just bytes

data WriteSocket m a where
    Write :: ByteString -> WriteSocket m ()

makeSem ''WriteSocket

runWrite :: Member (Embed IO) r => SemSocket -> Sem (WriteSocket ': r) a -> Sem r a
runWrite (SemSocket socket) = interpret $ \case
    Write bs -> embed $ sendAll socket bs

data SocketInfo m a where
    GetPeerName :: SocketInfo m SockAddr

makeSem ''SocketInfo

runSocketInfo :: Member (Embed IO) r => SemSocket -> Sem (SocketInfo ': r) a -> Sem r a
runSocketInfo (SemSocket socket) = interpret $ \case
    GetPeerName -> embed $ S.getPeerName socket

data HandleSocket m a where
    Open :: AddrInfo -> HandleSocket m SemSocket
    Close :: SemSocket -> HandleSocket m ()
    Loop :: SemSocket -> HandleSocket m ()

makeSem ''HandleSocket

runHandleSocket :: Member (Embed IO) r => Sem (ReadSocket ': WriteSocket ': SocketInfo ': r) a  -> Sem (HandleSocket ': r) a  -> Sem r a
runHandleSocket action = interpret $ \case
    Close (SemSocket sock) -> embed $ gracefulClose sock 5000
    Open addr -> embed $ bracketOnError (openSocket addr) S.close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        pure (SemSocket sock)
    Loop (SemSocket sock) -> embed $
        vacuous $
            infinitely $
                bracketOnError (accept sock) (S.close . fst) $
                    \(conn, _) ->
                        void $ do
                            let sem = SemSocket conn
                            let actionAsIO = runSocketInfo sem . runWrite sem .runRead sem $ action
                            undefined
                            -- forkFinally actionAsIO (const $ close sem)
