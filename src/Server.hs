{-# LANGUAGE OverloadedStrings #-}

module Server where

import DataTypes
import Packets
import Prelude hiding (max)

handlePacket :: IncomingPacket -> IO (Maybe OutgoingPacket)
handlePacket (Handshake _ _ _ nextState) = do
  case nextState of
    2 -> error "Not supported yet"
    1 -> pure Nothing
    _ -> error "WHAT"
handlePacket Request = do
  return . Just $
    Response x
handlePacket (Ping payload) = do
  return . Just $ Pong payload


x =  PingResponse
        { version = newVersion "1.19" 759,
          players =
            Players
              { max = 100,
                online = 0,
                sample = []
              },
          description =
            (chat "Haskell good kotlin bad")
              { color = HexString "c4451d"
              }
        }    