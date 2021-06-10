{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Vk
  ( startBotWithLogger
  ) where

import Control.Monad (replicateM_, void)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import Data.Time.Clock.System (getSystemTime)
import Prelude hiding (drop, id)
import System.Exit (exitFailure, exitSuccess)
import System.Log.Logger
  ( Priority(DEBUG, ERROR)
  , debugM
  , errorM
  , infoM
  , setLevel
  , traplogging
  , updateGlobalLogger
  )

import qualified Vk.Logger as L
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
  L.hInfo loggerH "Bot is up and running." >> getLongPollServerInfo config >>= \serverInfo ->
    L.hDebug loggerH (show serverInfo) >>
    cycleProcessing' loggerH config serverInfo

processArgs :: [String] -> Either String Config
processArgs [token, groupId, helpMsg, repeatMsg, echoRepeatNumberStr] =
  let echoRepeatNumber = (read echoRepeatNumberStr :: Int)
      isInRange n = n > 0 && n < 6
   in if or
           [ null token
           , null groupId
           , null helpMsg
           , null repeatMsg
           , not $ isInRange echoRepeatNumber
           ]
        then Left "Some argument passed from command line is wrong."
        else Right
               ( pack token
               , pack groupId
               , pack helpMsg
               , pack repeatMsg
               , pack echoRepeatNumberStr
               , M.empty)
processArgs _ =
  Left
    "Exactly five arguments needed: access token, group id, helpMsg, repeatMsg, echoRepeatNumber."

startBot :: L.Handle () -> [String] -> IO ()
startBot loggerH args =
  case processArgs args of
    Right config -> void $ cycleProcessing loggerH config >> exitSuccess
    Left errorMessage -> L.hError loggerH errorMessage >> exitFailure

startBotWithLogger :: [String] -> IO ()
startBotWithLogger args = do
  L.withLogger
    (L.Config
                -- use INFO, DEBUG or ERROR here
                -- (add to System.Log.Logger import items if missed)
       DEBUG
       (traplogging "trial-bot-vk" ERROR "Unhandled exception occured" .
        updateGlobalLogger "trial-bot-vk" . setLevel)
       (debugM "trial-bot-vk")
       (infoM "trial-bot-vk")
       (errorM "trial-bot-vk"))
    (\loggerH -> startBot loggerH args)
  pure ()
