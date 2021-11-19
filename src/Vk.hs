{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Vk
  ( startBot
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO, try)
import Control.Monad (replicateM_)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack, unpack)
import Data.Text.Read (decimal)
import Data.Time.Clock.System (getSystemTime)
import Network.HTTP.Req (HttpException(VanillaHttpException))
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

responseToText :: L.Handle () -> BotParams -> Update -> Text -> Int -> IO ()
responseToText loggerH botParams update msgText echoRepeatNumber =
  if isMsgTextHelpCommand msgText || isMsgTextRepeatCommand msgText
    then sendAndLog loggerH botParams update
    else replicateM_ echoRepeatNumber $ sendAndLog loggerH botParams update


processNewMessageUpdateWithText ::
    L.Handle ()
    -> (Config, NumberOfRepeatsMap)
    -> (Int -> IO ())
    -> PrivateMessage
    -> IO (Config, M.Map FromId Text)
processNewMessageUpdateWithText loggerH botParams@(config, numberOfRepeatsMap) responseToText' msg = 
    let maybeNewEchoRepeatNumberText = payload msg
        (_, _, _, _, defaultNumberOfRepeatsText) = config
    in case maybeNewEchoRepeatNumberText of
        Just newEchoRepeatNumberText ->
            let newNumberOfRepeatsMap =
                    M.insert
                      (from_id msg)
                      newEchoRepeatNumberText
                      numberOfRepeatsMap
                newBotParams =
                  ( config
                  , newNumberOfRepeatsMap)
            in L.hDebug loggerH ("echoRepeatNumber is set to " ++ unpack newEchoRepeatNumberText)
                >> return newBotParams
        _ -> let echoRepeatNumber = getInt
                    $ M.findWithDefault defaultNumberOfRepeatsText (from_id msg) numberOfRepeatsMap
            in responseToText' echoRepeatNumber
                >> return botParams


processNewMessageUpdate ::
    L.Handle ()
    -> (Config, NumberOfRepeatsMap)
    -> Update
    -> PrivateMessage
    -> IO (Config, NumberOfRepeatsMap)
processNewMessageUpdate loggerH botParams latestUpdateWithNewMessage msg = 
    let msgText = text msg
        isMsgHasNoText = msgText == ""
        responseToText' =
            responseToText
              loggerH
              botParams
              latestUpdateWithNewMessage
              msgText
    in if isMsgHasNoText 
        then return botParams
        else processNewMessageUpdateWithText loggerH botParams responseToText' msg


processUpdates :: L.Handle () -> BotParams -> [Update] -> IO BotParams
processUpdates loggerH botParams updates' =
  let updatesWithNewMessage = filter isMessageNew updates'
      latestUpdateWithNewMessage = last updatesWithNewMessage
      msg = getMessage latestUpdateWithNewMessage
   in if null updatesWithNewMessage
        then return botParams
        else processNewMessageUpdate loggerH botParams latestUpdateWithNewMessage msg


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


guardedInitLPProcessing :: L.Handle () -> BotParams -> IO LPResponse
guardedInitLPProcessing loggerH botParams =
    do
    eitherReqError <- try (initLPProcessing loggerH botParams)
    case eitherReqError of
        Left (VanillaHttpException e) ->
            L.hError loggerH "Seems like network is down. Trying another request in 2 seconds."
                >> L.hDebug loggerH (show e)
                    >> threadDelay 2000000
                        >> guardedInitLPProcessing loggerH botParams
        Left jsonHttpException -> throwIO jsonHttpException
        Right _ -> exitSuccess


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
            >> guardedInitLPProcessing loggerH botParams
                >> exitSuccess
        Left errorMessage -> L.hError loggerH errorMessage
            >> exitFailure
