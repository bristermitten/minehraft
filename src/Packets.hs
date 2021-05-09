module Packets where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.Serialize
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word
import DataTypes

data IncomingPacket
  = Handshake VarInt T.Text Word8 VarInt
  | Request
  | Ping Word64
  deriving (Show, Eq)

data OutgoingPacket
  = Response Chat
  | Pong

data PacketState = Handshaking | Status | Login | Play

packetOutId :: OutgoingPacket -> (PacketState, Int)
packetOutId p
  | Response _ <- p = (Status, 0x00)
  | Pong <- p = (Status, 0x01)
  | otherwise = error "Unknown packet ID "

packetInId :: IncomingPacket -> (PacketState, Int)
packetInId p
  | Handshake {} <- p = (Handshaking, 0x00)
  | Request <- p = (Status, 0x00)
  | Ping _ <- p = (Status, 0x01)
  | otherwise = error "Unknown packet ID "

getVarInt :: Get VarInt
getVarInt = do
  bs <- loop
  return $! toInt bs
  where
    loop = do
      b <- getWord8
      if b >= 0x80
        then do
          bs <- loop
          -- Clear off the continuation bit.
          return $ (b .&. 0x7f) : bs
        else return [b]
    toInt = foldr (\b i -> (i `shiftL` 7) .|. toInteger b) 0

getString :: Get T.Text
getString = do
  len <- getVarInt
  byteString <- getByteString $ fromInteger len
  return $ decodeUtf8 byteString

getPacket :: Get IncomingPacket
getPacket = do
  _ <- getVarInt
  packetId <- getVarInt
  case packetId of
    0x00 -> do
      protocolVersion <- getVarInt
      address <- getString
      port <- getWord8
      nextState <- getVarInt
      return $! Handshake protocolVersion address port nextState
    _ -> error $ "Can't decode packet with id: " ++ show packetId
