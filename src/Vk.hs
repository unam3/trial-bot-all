{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Vk
  ( startBot
  ) where

import Control.Monad (replicateM_)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import Data.Time.Clock.System (getSystemTime)
import Prelude hiding (drop, id)
import System.Exit (exitFailure, exitSuccess)

import Config (VkConfig(..))
import qualified Logger as L
import Vk.Requests
import Vk.Requests.JSON
import Vk.Types

isMessageNew :: Update -> Bool
isMessageNew = (== "message_new") . (_type :: Update -> Text)

getInt :: Text -> Int
getInt = fst . fromRight (1, "1") . decimal

sendAndLog :: L.Handle () -> Config -> Update -> IO ()
sendAndLog loggerH config update =
  getSystemTime >>= sendMessage config update >>= L.hDebug loggerH . show

responseToText :: L.Handle () -> Config -> Update -> Int -> Text -> IO ()
responseToText loggerH config update echoRepeatNumber msgText =
  if isMsgTextHelpCommand msgText || isMsgTextRepeatCommand msgText
    then sendAndLog loggerH config update
    else replicateM_ echoRepeatNumber $ sendAndLog loggerH config update

processUpdates :: L.Handle () -> Config -> [Update] -> IO Config
processUpdates loggerH config updates' =
  let (tokenSection, groupId, helpMsg, repeatMsg, echoRepeatNumberText, numberOfRepeatsMap) =
        config
      newMessages = filter isMessageNew updates'
      latestMessage = last newMessages
      msg = getMessage latestMessage
      msgText = text msg
      isMsgHasNoText = msgText == ""
      maybeNewEchoRepeatNumberText = payload msg
      newNumberOfRepeatsMap =
        M.insert
          (from_id msg)
          (fromJust maybeNewEchoRepeatNumberText)
          numberOfRepeatsMap
      echoRepeatNumber =
        getInt $
        M.findWithDefault echoRepeatNumberText (from_id msg) numberOfRepeatsMap
      newConfig =
        ( tokenSection
        , groupId
        , helpMsg
        , repeatMsg
        , echoRepeatNumberText
        , newNumberOfRepeatsMap)
   in if null newMessages || isMsgHasNoText
        then return config
        else if isJust maybeNewEchoRepeatNumberText
               then return newConfig
               else responseToText
                      loggerH
                      config
                      latestMessage
                      echoRepeatNumber
                      msgText >>
                    return config

cycleProcessing' :: L.Handle () -> Config -> LPServerInfo -> IO LPResponse
cycleProcessing' loggerH config serverInfo =
  getLongPoll serverInfo >>= \lp ->
    L.hDebug loggerH (show lp) >> processUpdates loggerH config (updates lp) >>= \newConfig ->
      cycleProcessing'
        loggerH
        newConfig
        serverInfo {ts = (ts :: LPResponse -> Text) lp}

cycleProcessing :: L.Handle () -> Config -> IO LPResponse
cycleProcessing loggerH config =
  getLongPollServerInfo config >>= \serverInfo ->
    L.hDebug loggerH (show serverInfo) >>
    cycleProcessing' loggerH config serverInfo

processConfig :: VkConfig -> Either String Config
processConfig (VkConfig token groupId helpMsg repeatMsg echoRepeatNumber) =
  let isInRange n = n > 0 && n < 6
   in if not $ isInRange echoRepeatNumber
        then Left
               "Number of message repeats (echoRepeatNumber) must be 1, 2, 3, 4 or 5."
        else Right
               ( token
               , groupId
               , helpMsg
               , repeatMsg
               , pack $ show echoRepeatNumber
               , M.empty)

startBot :: L.Handle () -> VkConfig -> IO ()
startBot loggerH parsedConfig =
  case processConfig parsedConfig of
    Right config ->
      L.hInfo loggerH "Vkontakte bot is up and running." >>
      cycleProcessing loggerH config >>
      exitSuccess
    Left errorMessage -> L.hError loggerH errorMessage >> exitFailure
