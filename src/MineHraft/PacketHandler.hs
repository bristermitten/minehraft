module MineHraft.PacketHandler where

import MineHraft.Packet.Types (GenericIncomingPacket (..), GenericOutgoingPacket (..), IncomingHandshakingPacket (..), IncomingStatusPacket (..), OutgoingStatusPacket (..), PacketState (..))
import MineHraft.Session (Session (..))
import MineHraft.Types.Chat (Chat (..), Color (..), chat)
import MineHraft.Types.Ping (Players (..), StatusResponseData (StatusResponseData), Version (..), description, players, version)
import Polysemy
import Polysemy.State
import Prelude hiding (State, max, modify)

handlePacket :: Member (State Session) r => GenericIncomingPacket -> Sem r (Maybe GenericOutgoingPacket)
handlePacket (IncomingHandshakingPacket (Handshake{_handshakeNextState})) = do
    let nextState = case _handshakeNextState of
            1 -> Status
            2 -> Login
            _ -> error "Invalid state"
    modify (\s -> s{_sessionState = nextState})
    pure Nothing
handlePacket (IncomingStatusPacket StatusRequest) =
    pure $
        Just $
            OutgoingStatusPacket $
                StatusResponse $
                    StatusResponseData
                        { version = Version "1.19" 759
                        , players =
                            Players
                                { max = 100
                                , online = 0
                                , sample = []
                                }
                        , description =
                            (chat "Haskell good kotlin bad")
                                { color = Just $ HexString "c4451d"
                                }
                        }
handlePacket (IncomingStatusPacket (Ping i)) = pure $ Just $ OutgoingStatusPacket $ Pong i
handlePacket other = error $ "Not implemented: " <> show other