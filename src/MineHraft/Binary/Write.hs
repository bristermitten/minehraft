{-# LANGUAGE TemplateHaskell #-}

module MineHraft.Binary.Write where

import Data.Binary (Put)
import Data.Binary.Put (runPut)
import Polysemy
import Polysemy.State qualified as PS

data WriteBinary m a where
    Write :: Put -> WriteBinary m ()

makeSem ''WriteBinary

liftPut :: Member WriteBinary r => Put -> Sem r ()
liftPut = send . Write


runWriteBinary :: Sem (WriteBinary ': r) a -> Sem r (LByteString, a)
runWriteBinary ps =
    PS.runState
        ""
        (reinterpret (\(Write b) -> PS.modify (<> runPut b)) ps)


execWriteBinary :: Sem (WriteBinary ': r) a -> Sem r LByteString
execWriteBinary = fmap fst . runWriteBinary