module Server where

import DataTypes
import Packets

handlePacket :: IncomingPacket -> IO (Maybe OutgoingPacket)
handlePacket (Handshake version address port nextState) = do
  print address
  if nextState == 2
    then error "Not supported yet"
    else
      if nextState /= 1
        then return Nothing
        else error "WHAT"
handlePacket Request = do
  return . Just $
    Response
      Chat
        { text = "Hello"
        }
