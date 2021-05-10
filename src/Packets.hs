module Packets where

import Control.Monad
import qualified Data.Aeson as A
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Serialize
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word
import DataTypes
import Debug.Trace
import Math

data IncomingPacket
  = Handshake VarInt T.Text Word16 VarInt
  | Request
  | Ping Word64
  deriving (Show, Eq)

data OutgoingPacket
  = Response PingResponse
  | Pong
  deriving (Show)

data PacketState = Handshaking | Status | Login | Play deriving (Show)

packetStateFrom :: (Eq a, Num a) => a -> PacketState
packetStateFrom n
  | n == 0 = Handshaking
  | n == 1 = Status
  | n == 2 = Login
  | n == 3 = Play
  | otherwise = error "Invalid packet state"

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
    -- And there are only seven bits here, so shift by seven, not eight.
    toInt = foldr (\b i -> (i `shiftL` 7) .|. toInteger b) 0

-- This is pretty much a direct translation of the imperative counterpart and so likely isn't very idiomatic
putVarInt :: Putter VarInt
putVarInt = loop
  where
    loop value = do
      if value .&. (-128) == 0
        then putWord8 $ unsafeToWord value
        else do
          let toWrite = unsafeToWord (value .&. 127 .|. 128)
          putWord8 toWrite
          let shiftedValue = value `uShiftR` 7
          loop shiftedValue

getString :: Get T.Text
getString = do
  len <- getVarInt
  byteString <- getByteString $ fromInteger len
  return $ decodeUtf8 byteString

putText :: Putter T.Text
putText = putString . encodeUtf8

putString :: Putter BS.ByteString
putString s = do
  putVarInt $ toInteger $ BS.length s
  putByteString s

getPacket :: PacketState -> Get IncomingPacket
getPacket state = do
  _ <- getVarInt
  packetId <- getVarInt
  case (state, packetId) of
    (Handshaking, 0x00) -> do
      protocolVersion <- getVarInt
      address <- getString
      port <- getWord16be
      nextState <- getVarInt
      return $! Handshake protocolVersion address port nextState
    (Status, 0x00) -> return Request
    (Status, 0x01) -> do
      payload <- getWord64be
      return $! Ping payload
    _ -> fail $ "Can't decode packet with id: " ++ show packetId

-- |
putPacketHeaders :: Word8 -> Put -> Put
putPacketHeaders packetId packetData = do
  let packet = runPut packetData
  traceM (show packet)
  traceM ("Length = " ++ show (BS.length packet))
  let len = toInteger $ 1 + BS.length packet
  putVarInt len
  putVarInt $ toInteger packetId
  putByteString packet

putPacket :: Putter OutgoingPacket
putPacket (Response msg) = putPacketHeaders 0x00 $ do
  let encoded = A.encode msg
  putString $ toStrict encoded
