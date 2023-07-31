{-# LANGUAGE DuplicateRecordFields #-}

module MineHraft.Types.Ping where

import Data.Aeson
import MineHraft.Types.Chat (Chat)

data StatusResponseData = StatusResponseData
    { version :: Version
    , players :: Players
    , description :: Chat
    }
    deriving (Show, Generic, Eq)

instance ToJSON StatusResponseData where
    toEncoding = genericToEncoding defaultOptions

data Version = Version
    { name :: String
    , protocol :: Int
    }
    deriving (Show, Generic, Eq)

instance ToJSON Version where
    toEncoding = genericToEncoding defaultOptions

data Players = Players
    { max :: Int
    , online :: Int
    , sample :: [Player]
    }
    deriving (Show, Generic, Eq)

instance ToJSON Players where
    toEncoding = genericToEncoding defaultOptions

data Player = Player
    { name :: String
    , id :: String -- todo UUID type
    }
    deriving (Show, Generic, Eq)

instance ToJSON Player where
    toEncoding = genericToEncoding defaultOptions