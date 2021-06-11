{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..),
  TgConfig (..),
  VkConfig (..),
  parseConfig
  ) where

import Data.Ini.Config
import qualified Data.Text.IO as T


data Config = Config {
    c_run :: WhatToRun,
    c_tg :: Maybe TgConfig,
    c_vk :: Maybe VkConfig
} deriving (Eq, Show)

data WhatToRun = Tg | Vk deriving (Eq, Show)

data TgConfig = TgConfig {
    tg_token :: String,
    tg_helpMsg :: String,
    tg_repeatMsg :: String,
    tg_echoRepeatNumber :: Int
} deriving (Eq, Show)

data VkConfig = VkConfig {
    vk_token :: String,
    vk_groupId :: String,
    vk_helpMsg :: String,
    vk_repeatMsg :: String,
    vk_echoRepeatNumber :: Int
} deriving (Eq, Show)

configParser :: IniParser Config
configParser = do
  run <- section "WhatToRun" $ do
    run <- fieldOf "run" string
    return $ (if run == "tg" then Tg else Vk)

  tg <- sectionMb "Telegram" $ do
    token <- fieldOf "token" string
    helpMsg <- fieldOf "helpMsg" string
    repeatMsg <- fieldOf "repeatMsg" string
    echoRepeatNumber <- fieldOf "echoRepeatNumber" number
    return $ TgConfig
        token
        helpMsg
        repeatMsg
        echoRepeatNumber
  vk <- sectionMb "Vkontakte" $ do
    token <- fieldOf "token" string
    groupId <- fieldOf "groupId" string
    helpMsg <- fieldOf "helpMsg" string
    repeatMsg <- fieldOf "repeatMsg" string
    echoRepeatNumber <- fieldOf "echoRepeatNumber" number
    return $ VkConfig
        token
        groupId
        helpMsg
        repeatMsg
        echoRepeatNumber
  return Config { c_run = run, c_tg = tg, c_vk = vk }


parseConfig :: IO (Either String Config)
parseConfig = T.readFile "config.ini"
    >>= pure . (`parseIniFile`  configParser)

