{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

type VarInt = Integer

data Chat = Chat
  { text :: String,
    bold :: Bool,
    italic :: Bool,
    underlined :: Bool,
    strikethrough :: Bool,
    obfuscated :: Bool,
    color :: Color,
    insertion :: String,
    clickEvent :: ClickEvent,
    hoverEvent :: HoverEvent,
    extra :: Maybe [Chat]
  }
  deriving (Generic, Show)

data Color = Black | DarkBlue | DarkGreen | DarkAqua | DarkRed | DarkPurple | Gold | Gray | DarkGray | Blue | Green | Aqua | Red | LightPurple | Yellow | White | HexString {value :: T.Text} deriving (Show)

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
