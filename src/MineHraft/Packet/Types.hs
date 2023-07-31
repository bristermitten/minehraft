{-# LANGUAGE TemplateHaskell #-}

module MineHraft.Packet.Types where

import Control.Lens
import Control.Lens.TH ()
import MineHraft.Types.Ping (StatusResponseData)

data PacketState = Handshaking | Status | Login | Play deriving (Show, Eq)

type VarInt = Int32

data IncomingHandshakingPacket
    = Handshake
        { _handshakeProtocolVersion :: !VarInt
        , _handshakeServerAddress :: !Text
        , _handshakeServerPort :: !Word16
        , _handshakeNextState :: !VarInt
        }
    deriving (Show, Eq)

data IncomingStatusPacket
    = StatusRequest
    | Ping !Word64
    deriving (Show, Eq)

data GenericIncomingPacket
    = IncomingHandshakingPacket !IncomingHandshakingPacket
    | IncomingStatusPacket !IncomingStatusPacket
    deriving (Show, Eq)

makeClassy ''IncomingHandshakingPacket
makeClassy ''IncomingStatusPacket
makeClassy ''GenericIncomingPacket

data OutgoingStatusPacket
    = StatusResponse !StatusResponseData
    | Pong !Word64
    deriving (Show, Eq)

data GenericOutgoingPacket
    = OutgoingStatusPacket OutgoingStatusPacket
    deriving (Show, Eq)

makeClassy ''OutgoingStatusPacket
makeClassy ''GenericOutgoingPacket

class HasState a where
    state :: a -> PacketState
