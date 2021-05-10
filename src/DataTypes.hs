{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

type VarInt = Integer

data PingResponse = PingResponse
  { version :: Version,
    players :: Players,
    description :: Chat
  }
  deriving (Show, Generic)

instance ToJSON PingResponse where
  toEncoding = genericToEncoding defaultOptions

data Version = Version
  { name :: String,
    protocol :: Int
  }
  deriving (Show, Generic)

-- Yes, I know lenses exist and this is a bad practice but i am lazy and want instant gratification
newVersion :: String -> Int -> Version
newVersion = Version

instance ToJSON Version where
  toEncoding = genericToEncoding defaultOptions

data Players = Players
  { max :: Int,
    online :: Int,
    sample :: [Player]
  }
  deriving (Show, Generic)

instance ToJSON Players where
  toEncoding = genericToEncoding defaultOptions

data Player = Player
  { name :: String,
    id :: String -- todo UUID type
  }
  deriving (Show, Generic)

instance ToJSON Player where
  toEncoding = genericToEncoding defaultOptions

data Chat = Chat
  { text :: String,
    bold :: Bool,
    italic :: Bool,
    underlined :: Bool,
    strikethrough :: Bool,
    obfuscated :: Bool,
    color :: Color,
    insertion :: String,
    clickEvent :: Maybe ClickEvent,
    hoverEvent :: Maybe HoverEvent,
    extra :: Maybe [Chat]
  }
  deriving (Generic, Show)

chat :: String -> Chat
chat msg =
  Chat
    { text = msg,
      bold = False,
      italic = False,
      strikethrough = False,
      obfuscated = False,
      underlined = False,
      color = White,
      insertion = "",
      clickEvent = Nothing,
      hoverEvent = Nothing,
      extra = Nothing
    }

instance ToJSON Chat where
  toEncoding =
    genericToEncoding
      defaultOptions
        { omitNothingFields = True
        }

data Color = Black | DarkBlue | DarkGreen | DarkAqua | DarkRed | DarkPurple | Gold | Gray | DarkGray | Blue | Green | Aqua | Red | LightPurple | Yellow | White | HexString T.Text deriving (Show)

instance ToJSON Color where
  toJSON :: Color -> Value
  toJSON (HexString hex) = String (T.cons '#' hex)
  toJSON Black = "black"
  toJSON DarkBlue = "dark_blue"
  toJSON DarkGreen = "dark_green"
  toJSON DarkAqua = "dark_aqua"
  toJSON DarkRed = "dark_red"
  toJSON DarkPurple = "dark_purple"
  toJSON Gold = "gold"
  toJSON Gray = "gray"
  toJSON DarkGray = "dark_gray"
  toJSON Blue = "blue"
  toJSON Green = "green"
  toJSON Aqua = "aqua"
  toJSON Red = "red"
  toJSON LightPurple = "light_purple"
  toJSON Yellow = "yellow"
  toJSON White = "white"

data ClickEvent = ClickEvent
  { clickAction :: ClickEventAction,
    clickValue :: String
  }
  deriving (Show)

instance ToJSON ClickEvent where
  toJSON (ClickEvent action value) = object ["action" .= action, "value" .= value]

data ClickEventAction = OpenURL | RunCommand | SuggestCommand | ChangePage | CopyToClipboard deriving (Show)

instance ToJSON ClickEventAction where
  toJSON :: ClickEventAction -> Value
  toJSON OpenURL = "open_url"
  toJSON RunCommand = "run_command"
  toJSON SuggestCommand = "suggest_command"
  toJSON ChangePage = "change_page"
  toJSON CopyToClipboard = "copy_to_clipboard"

data HoverEvent = HoverEvent
  { hoverAction :: HoverEventAction,
    hoverValue :: String -- TODO this probably needs to be a component
  }
  deriving (Show)

instance ToJSON HoverEvent where
  toJSON (HoverEvent action value) = object ["action" .= action, "value" .= value]

data HoverEventAction = ShowText | ShowItem | ShowEntity deriving (Show)

instance ToJSON HoverEventAction where
  toJSON :: HoverEventAction -> Value
  toJSON ShowText = "show_text"
  toJSON ShowItem = "show_item"
  toJSON ShowEntity = "show_entity"
