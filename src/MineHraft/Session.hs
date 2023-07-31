{-# LANGUAGE TemplateHaskell #-}

module MineHraft.Session where

import Control.Lens (makeClassy)
import MineHraft.Packet.Types
import Network.Socket (SockAddr)

type SessionName = SockAddr -- This is not great

data Session = Session
    { _sessionName :: SessionName
    , _sessionState :: PacketState
    , _sessionCompressionThreshold :: Maybe Int
    , _sessionEncryptionEnabled :: Bool
    }
    deriving (Show, Eq)

makeClassy ''Session

newSession :: SessionName -> Session
newSession name =
    Session
        { _sessionName = name
        , _sessionState = Handshaking
        , _sessionCompressionThreshold = Nothing
        , _sessionEncryptionEnabled = False
        }