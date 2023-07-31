{-# LANGUAGE TemplateHaskell #-}

module MineHraft.Packet.Binary (getPacket, putPacket) where

import Control.Lens ((^.))
import Control.Lens.TH
import Data.Binary (Get, Put, putWord8)
import Data.Binary.Get (getWord64be, getWord8, runGet)
import Data.Binary.Put (putByteString, putLazyByteString, putWord64be, runPut)
import Data.Bits (Bits (shiftL, (.|.)), (.&.))
import Data.ByteString.Lazy qualified as LBS
import MineHraft.Binary.Read
import MineHraft.Binary.Write
import MineHraft.Math (uShiftR, unsafeToWord)
import MineHraft.Packet.Types
import Polysemy

import Data.Aeson qualified as A
import Data.ByteString qualified as BS

data RawPacket = Uncompressed
    { _packetLength :: !VarInt
    , _packetData :: !BS.ByteString
    }
    deriving (Show)
makeLenses ''RawPacket

type Get' r a = Member ReadBinary r => Sem r a

getRawPacket :: Bool -> Get' r RawPacket
getRawPacket False = do
    len <- getVarInt
    rawData <- getByteString (fromIntegral len)
    pure $ Uncompressed len rawData
getRawPacket True = error "Compressed packets not yet supported."

getPacket :: Member ReadBinary r => Bool -> PacketState -> Sem r GenericIncomingPacket
getPacket compression s = do
    raw <- getRawPacket compression
    runReadBinary (fromStrict $ raw ^. packetData) $ decode s
  where
    decode Handshaking = IncomingHandshakingPacket <$> decodeHandshaking
    decode Status = IncomingStatusPacket <$> decodeStatus
    decode Login = error "Login packets not yet supported."
    decode Play = error "Play packets not yet supported."

decodeHandshaking :: Get' r IncomingHandshakingPacket
decodeHandshaking = Handshake <$> getVarInt <*> getString <*> getWord16be <*> getVarInt

decodeStatus :: Get' r IncomingStatusPacket
decodeStatus = Ping <$> getLong

-- This is pretty much a direct translation of the imperative counterpart and so likely isn't very idiomatic
putVarInt :: VarInt -> Put
putVarInt = loop
  where
    loop :: VarInt -> Put
    loop value = do
        if value .&. (-128) == 0
            then putWord8 $ unsafeToWord value
            else do
                let toWrite = unsafeToWord (value .&. 127 .|. 128)
                putWord8 toWrite
                let shiftedValue = value `uShiftR` 7
                loop shiftedValue

putString :: Text -> Put
putString s = do
    let bs = encodeUtf8 s
    putVarInt $ fromIntegral $ BS.length bs
    putByteString bs

putPacket :: Member WriteBinary r => Bool -> GenericOutgoingPacket -> Sem r ()
putPacket compression packet = liftPut $ putPacket' compression packet

putPacket' :: Bool -> GenericOutgoingPacket -> Put
putPacket' compression packet = do
    let packetData' = runPut (encode packet)
    putVarInt $ fromIntegral $ LBS.length packetData'
    putLazyByteString packetData'
  where
    encode (OutgoingStatusPacket o) = encodeStatus o
    encode other = error $ "Encoding of " <> show other <> " not yet supported."

encodeStatus :: OutgoingStatusPacket -> Put
encodeStatus (StatusResponse d) = do
    putVarInt 0x00
    putString $ decodeUtf8 $ A.encode d
encodeStatus (Pong p) = do
    putVarInt 0x01
    putWord64be p