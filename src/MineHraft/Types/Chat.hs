module MineHraft.Types.Chat where

import Data.Aeson
import Data.Text (cons)

data Chat = Chat
    { text :: Text
    , bold :: Maybe Bool
    , italic :: Maybe Bool
    , underlined :: Maybe Bool
    , strikethrough :: Maybe Bool
    , obfuscated :: Maybe Bool
    , color :: Maybe Color
    , insertion :: Maybe Text
    , clickEvent :: Maybe ClickEvent
    , hoverEvent :: Maybe HoverEvent
    , extra :: Maybe [Chat]
    }
    deriving (Generic, Show, Eq)

chat :: Text -> Chat
chat msg =
    Chat
        { text = msg
        , bold = Nothing
        , italic = Nothing
        , strikethrough = Nothing
        , obfuscated = Nothing
        , underlined = Nothing
        , color = Nothing
        , insertion = Nothing
        , clickEvent = Nothing
        , hoverEvent = Nothing
        , extra = Nothing
        }

instance ToJSON Chat where
    toEncoding =
        genericToEncoding
            defaultOptions
                { omitNothingFields = True
                }

data Color
    = Black
    | DarkBlue
    | DarkGreen
    | DarkAqua
    | DarkRed
    | DarkPurple
    | Gold
    | Gray
    | DarkGray
    | Blue
    | Green
    | Aqua
    | Red
    | LightPurple
    | Yellow
    | White
    | HexString Text
    deriving (Show, Eq)

instance ToJSON Color where
    toJSON :: Color -> Value
    toJSON (HexString hex) = String (cons '#' hex)
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
    { clickAction :: ClickEventAction
    , clickValue :: String
    }
    deriving (Show, Eq)

instance ToJSON ClickEvent where
    toJSON (ClickEvent action value) = object ["action" .= action, "value" .= value]

data ClickEventAction
    = OpenURL
    | RunCommand
    | SuggestCommand
    | ChangePage
    | CopyToClipboard
    deriving (Show, Eq)

instance ToJSON ClickEventAction where
    toJSON :: ClickEventAction -> Value
    toJSON OpenURL = "open_url"
    toJSON RunCommand = "run_command"
    toJSON SuggestCommand = "suggest_command"
    toJSON ChangePage = "change_page"
    toJSON CopyToClipboard = "copy_to_clipboard"

data HoverEvent = HoverEvent
    { hoverAction :: HoverEventAction
    , hoverValue :: String -- TODO this probably needs to be a component
    }
    deriving (Show, Eq)

instance ToJSON HoverEvent where
    toJSON (HoverEvent action value) = object ["action" .= action, "value" .= value]

data HoverEventAction
    = ShowText
    | ShowItem
    | ShowEntity
    deriving (Show, Eq)

instance ToJSON HoverEventAction where
    toJSON :: HoverEventAction -> Value
    toJSON ShowText = "show_text"
    toJSON ShowItem = "show_item"
    toJSON ShowEntity = "show_entity"