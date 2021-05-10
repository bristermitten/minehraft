module Server where

import DataTypes
import Packets
import Prelude hiding (max)

handlePacket :: IncomingPacket -> IO (Maybe OutgoingPacket)
handlePacket (Handshake _ _ _ nextState) = do
  case nextState of
    2 -> error "Not supported yet"
    1 -> return Nothing
    _ -> error "WHAT"
handlePacket Request = do
  return . Just $
    Response
      PingResponse
        { version = newVersion "1.16.5" 754,
          players =
            Players
              { max = 100,
                online = 0,
                sample = []
              },
          description = chat "Hello Haskell!"
        }
