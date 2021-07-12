{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Vk
  ( startBot
  ) where

import Control.Monad (replicateM_)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
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

sendAndLog :: L.Handle () -> BotParams -> Update -> IO ()
sendAndLog loggerH botParams update =
  getSystemTime
    >>= sendMessage botParams update
        >>= L.hDebug loggerH . show

responseToText :: L.Handle () -> BotParams -> Update -> Int -> Text -> IO ()
responseToText loggerH botParams update echoRepeatNumber msgText =
  if isMsgTextHelpCommand msgText || isMsgTextRepeatCommand msgText
    then sendAndLog loggerH botParams update
    else replicateM_ echoRepeatNumber $ sendAndLog loggerH botParams update

processUpdates :: L.Handle () -> BotParams -> [Update] -> IO BotParams
processUpdates loggerH botParams@(config, numberOfRepeatsMap) updates' =
  let (_, _, _, _, defaultNumberOfRepeatsText) = config
      newMessages = filter isMessageNew updates'
      latestMessage = last newMessages
      msg = getMessage latestMessage
      msgText = text msg
      isMsgHasNoText = msgText == ""
      maybeNewEchoRepeatNumberText = payload msg
      echoRepeatNumber =
        getInt $
        M.findWithDefault defaultNumberOfRepeatsText (from_id msg) numberOfRepeatsMap
   in if null newMessages || isMsgHasNoText
        then return botParams
        else case maybeNewEchoRepeatNumberText of
            Just newEchoRepeatNumberText ->
                let newNumberOfRepeatsMap =
                        M.insert
                          (from_id msg)
                          newEchoRepeatNumberText
                          numberOfRepeatsMap
                    newBotParams =
                      ( config
                      , newNumberOfRepeatsMap)
                in 
               return newBotParams
            _ -> responseToText
                  loggerH
                  botParams
                  latestMessage
                  echoRepeatNumber
                  msgText >>
                return botParams

cycleLPProcessing :: L.Handle () -> BotParams -> LPServerInfo -> IO LPResponse
cycleLPProcessing loggerH botParams serverInfo =
    getLongPoll serverInfo
        >>= \lpr -> L.hDebug loggerH (show lpr)
            >> case lpr of
                LPRSuccess ts' updates' -> processUpdates loggerH botParams updates'
                    >>= \newBotParams -> cycleLPProcessing
                        loggerH
                        newBotParams
                        serverInfo {ts = ts'}
                -- https://vk.com/dev/bots_longpoll?f=2.2.%20%D0%9E%D1%88%D0%B8%D0%B1%D0%BA%D0%B8
                -- failed 1
                LPRFailureWithTS ts' -> cycleLPProcessing
                    loggerH
                    botParams
                    serverInfo {ts = ts'}
                -- failed 2 and 3
                LPRFailure -> initLPProcessing
                    loggerH
                    botParams

initLPProcessing :: L.Handle () -> BotParams -> IO LPResponse
initLPProcessing loggerH botParams@(config, _) =
  getLongPollServerInfo config
    >>= \serverInfo -> L.hDebug loggerH (show serverInfo)
        >> cycleLPProcessing loggerH botParams serverInfo

processConfig :: VkConfig -> Either String BotParams
processConfig (VkConfig token groupId helpMsg repeatMsg echoRepeatNumber) =
    let {
        isInRange n = n > 0 && n < 6;
        config = (
            token
           , groupId
           , helpMsg
           , repeatMsg
           , pack $ show echoRepeatNumber
        );
        numberOfRepeatsMap = M.empty;
    } in if not $ isInRange echoRepeatNumber
        then Left
            "Number of message repeats (echoRepeatNumber) must be 1, 2, 3, 4 or 5."
        else Right (config, numberOfRepeatsMap)


startBot :: L.Handle () -> VkConfig -> IO ()
startBot loggerH parsedConfig =
    case processConfig parsedConfig of
        Right botParams -> L.hInfo loggerH "Vkontakte bot is up and running."
            >> initLPProcessing loggerH botParams
                >> exitSuccess
        Left errorMessage -> L.hError loggerH errorMessage
            >> exitFailure
