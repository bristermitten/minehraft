{-# LANGUAGE TemplateHaskell #-}

module MineHraft.Binary.Read where

import Data.Binary (Get)
import Data.Binary.Get (Decoder (..), runGet, runGetIncremental)
import Data.Binary.Get qualified as B
import Data.Bits (Bits (shiftL, (.|.)), (.&.))
import MineHraft.Packet.Types (VarInt)
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Socket (ReadSocket, nextN)

data ReadBinary m a where
    Read :: Get a -> ReadBinary m a

makeSem ''ReadBinary

liftGet :: Member ReadBinary r => Get a -> Sem r a
liftGet = send . Read

runReadBinary :: LByteString -> Sem (ReadBinary ': r) b -> Sem r b
runReadBinary bs = interpret $ \case
    Read g -> pure $ runGet g bs

runReadBinaryFromSocket :: (Member ReadSocket r, Member (Error Text) r) => Sem (ReadBinary ': r) a -> Sem r a
runReadBinaryFromSocket = interpret $ \case
    Read g -> go (runGetIncremental g)
  where
    go (Done _ _ a) = pure a
    go (Partial k) = do
        bytes <- nextN 1
        case bytes of
            Nothing -> error "No more bytes"
            Just bs -> go (k (Just bs))
    go (Fail _ _ err) = throw $ toText err

getByteString :: Member ReadBinary r => Int -> Sem r ByteString
getByteString n = liftGet $ B.getByteString n

getWord8 :: Member ReadBinary r => Sem r Word8
getWord8 = liftGet B.getWord8

getWord16be :: Member ReadBinary r => Sem r Word16
getWord16be = liftGet B.getWord16be

getWord32be :: Member ReadBinary r => Sem r Word32
getWord32be = liftGet B.getWord32be

getLong :: Member ReadBinary r => Sem r Word64
getLong = liftGet B.getWord64be

getString :: Member ReadBinary r => Sem r Text
getString = do
    len <- getVarInt
    decodeUtf8 <$> getByteString (fromIntegral len)

getVarInt :: Member ReadBinary r => Sem r VarInt
getVarInt = do
    bs <- loop
    pure $! toInt bs
  where
    loop :: Member ReadBinary r => Sem r [Word8]
    loop = do
        b <- getWord8
        if b >= 0x80
            then do
                -- Clear off the continuation bit.
                bs <- loop
                pure $ (b .&. 0x7f) : bs
            else pure [b]
    -- And there are only seven bits here, so shift by seven, not eight.
    toInt = foldr (\b i -> (i `shiftL` 7) .|. fromIntegral b) 0