{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , TgConfig(..)
  , VkConfig(..)
  , WhatToRun(..)
  , parseConfig
  ) where

import Data.Ini.Config
import Data.Text (Text)
import qualified Data.Text.IO as T

data Config =
  Config
    { c_run :: WhatToRun
    , c_tg :: Maybe TgConfig
    , c_vk :: Maybe VkConfig
    }
  deriving (Eq, Show)

data WhatToRun
  = Tg
  | Vk
  deriving (Eq, Show)

data TgConfig =
  TgConfig
    { tg_token :: Text
    , tg_helpMsg :: Text
    , tg_repeatMsg :: Text
    , tg_echoRepeatNumber :: Int
    }
  deriving (Eq, Show)

data VkConfig =
  VkConfig
    { vk_token :: Text
    , vk_groupId :: Text
    , vk_helpMsg :: Text
    , vk_repeatMsg :: Text
    , vk_echoRepeatNumber :: Int
    }
  deriving (Eq, Show)

configParser :: IniParser Config
configParser = do
  run <-
    section "WhatToRun" $ do
      run <- fieldOf "run" string
      return $
        (if run == "tg"
           then Tg
           else Vk)
  tg <-
    sectionMb "Telegram" $ do
      token <- field "token"
      helpMsg <- field "helpMsg"
      repeatMsg <- field "repeatMsg"
      echoRepeatNumber <- fieldOf "echoRepeatNumber" number
      return $ TgConfig token helpMsg repeatMsg echoRepeatNumber
  vk <-
    sectionMb "Vkontakte" $ do
      token <- field "token"
      groupId <- field "groupId"
      helpMsg <- field "helpMsg"
      repeatMsg <- field "repeatMsg"
      echoRepeatNumber <- fieldOf "echoRepeatNumber" number
      return $ VkConfig token groupId helpMsg repeatMsg echoRepeatNumber
  return Config {c_run = run, c_tg = tg, c_vk = vk}

parseConfig :: IO (Either String Config)
parseConfig = T.readFile "config.ini" >>= pure . (`parseIniFile` configParser)
