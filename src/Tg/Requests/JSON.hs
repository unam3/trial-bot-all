{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.Requests.JSON where

import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , (.:)
  , (.:?)
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , genericToJSON
  , omitNothingFields
  , withObject
  )
import Data.Text (Text)
import GHC.Generics (Generic)

import Tg.Types

data RequestJSON
  = WithoutOffset
      { timeout :: Int
      }
  | WithOffset
      { timeout :: Int
      , offset :: Offset
      }
  deriving (Show, Generic)

instance ToJSON RequestJSON

instance FromJSON RequestJSON

newtype Chat =
  Chat
    { id :: ChatID
    }
  deriving (Show, Generic)

instance ToJSON Chat

instance FromJSON Chat where
  parseJSON = withObject "Chat" $ \v -> Chat <$> v .: "id"

data User =
  User
    { _username :: Maybe Username
    , _id :: UserID
    }
  deriving (Show, Generic, Eq)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data Message =
  Message
    { chat :: Chat
    , text :: Maybe Text
    , from :: Maybe User
    }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON =
    withObject "Message" $ \v ->
      Message <$> v .: "chat" <*> v .:? "text" <*> v .:? "from"

data CallbackQuery =
  CallbackQuery
    { _id :: Text
    , _data :: Text
    , _from :: User
    }
  deriving (Show, Generic, Eq)

instance FromJSON CallbackQuery where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data Update =
  Update
    { update_id :: Offset
    , message :: Maybe Message
    , callback_query :: Maybe CallbackQuery
    }
  deriving (Show, Generic)

instance FromJSON Update

newtype ResponseStatusJSON =
  RSJSON
    { ok :: Bool
    }
  deriving (Show, Generic)

instance ToJSON ResponseStatusJSON

instance FromJSON ResponseStatusJSON where
  parseJSON = withObject "ResponseStatusJSON" $ \v -> RSJSON <$> v .: "ok"

data ResponseJSON =
  RJSON
    { ok :: Bool
    , result :: [Update]
    }
  deriving (Show, Generic)

instance FromJSON ResponseJSON where
  parseJSON =
    withObject "ResponseJSON" $ \v -> RJSON <$> v .: "ok" <*> v .: "result"

data InlineKeyboardButton =
  InlineKeyboardButton
    { text :: Text
    -- 1-64 bytes ( https://core.telegram.org/bots/api#inlinekeyboardbutton  )
    , callback_data :: Text
    }
  deriving (Show, Generic)

instance ToJSON InlineKeyboardButton

newtype InlineKeyboardMarkup =
  InlineKeyboardMarkup
    { inline_keyboard :: [[InlineKeyboardButton]]
    }
  deriving (Show, Generic)

instance ToJSON InlineKeyboardMarkup

data EchoRequest =
  EchoRequest
    { chat_id :: ChatID
    , text :: Text
    , reply_markup :: Maybe InlineKeyboardMarkup
    }
  deriving (Show, Generic)

instance ToJSON EchoRequest where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

newtype AnswerCallbackRequest =
  AnswerCallbackRequest
    { callback_query_id :: Text
    }
  deriving (Show, Generic)

instance ToJSON AnswerCallbackRequest
