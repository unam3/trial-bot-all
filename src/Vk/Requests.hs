{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Vk.Requests where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, breakOn, drop, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.System (SystemTime, systemNanoseconds)
import Network.HTTP.Req
import Prelude hiding (drop)

import Vk.Requests.JSON
import Vk.Types

makeRequest ::
     (MonadIO m, FromJSON a)
  => Url scheme
  -> Option scheme
  -> m (JsonResponse a)
makeRequest urlScheme botParams =
  runReq defaultHttpConfig $ req GET urlScheme NoReqBody jsonResponse botParams

getLongPollServerInfo :: Config -> IO LPServerInfo
getLongPollServerInfo (tokenSection, groupId, _, _, _) =
  let urlScheme = https "api.vk.com" /: "method" /: "groups.getLongPollServer"
      botParams =
        "v" =: ("5.110" :: Text) <>
        "access_token" =: tokenSection <> "group_id" =: groupId
   in (response :: LPServerInfoResponse -> LPServerInfo) . responseBody <$>
      makeRequest urlScheme botParams

getLongPoll :: LPServerInfo -> IO LPResponse
getLongPoll serverInfo
    -- https://lp.vk.com/wh123456789
 =
  let (serverName, wh) = breakOn "/" $ drop 8 (server serverInfo)
      urlScheme = https serverName /: drop 1 wh
      botParams =
        "act" =: ("a_check" :: Text) <>
        "key" =: key serverInfo <>
        "ts" =: (ts :: LPServerInfo -> Text) serverInfo <>
        "wait" =: ("25" :: Text)
   in responseBody <$> makeRequest urlScheme botParams

getMessage :: Update -> PrivateMessage
getMessage = (message :: Object -> PrivateMessage) . _object

isMsgTextHelpCommand :: Text -> Bool
isMsgTextHelpCommand = (== "/help")

isMsgTextRepeatCommand :: Text -> Bool
isMsgTextRepeatCommand = (== "/repeat")

keyboard :: Text
keyboard =
  decodeUtf8 . toStrict $
  encode
    Keyboard
      { one_time = True
      , buttons =
          [ [ Button
                {action = Action {_label = "1", _payload = "1", _type = "text"}}
            ]
          , [ Button
                {action = Action {_label = "2", _payload = "2", _type = "text"}}
            ]
          , [ Button
                {action = Action {_label = "3", _payload = "3", _type = "text"}}
            ]
          , [ Button
                {action = Action {_label = "4", _payload = "4", _type = "text"}}
            ]
          , [ Button
                {action = Action {_label = "5", _payload = "5", _type = "text"}}
            ]
          ]
      }

sendMessage :: BotParams -> Update -> SystemTime -> IO SendMessageResponse
-- TODO: include actual number of repeats into message
--sendMessage (config, numberOfRepeatsMap) update systemTime = 
sendMessage (config, _) update systemTime =
  let (tokenSection, _, helpMsg, repeatMsg, defaultNumberOfRepeatsText) = config
      incomingMessage = getMessage update
      urlScheme = https "api.vk.com" /: "method" /: "messages.send"
      msgText' = text incomingMessage
      msgText
        | isMsgTextHelpCommand msgText' = helpMsg
        | isMsgTextRepeatCommand msgText' =
          mconcat
            [ "Current number of repeats for you is "
            , defaultNumberOfRepeatsText
            , ". "
            , repeatMsg
            ]
        | otherwise = msgText'
      botParams' =
        "v" =: ("5.110" :: Text) <>
        "access_token" =: tokenSection <>
        "group_id" =: _group_id update <>
        "peer_id" =: peer_id incomingMessage <>
        "random_id" =: pack (show $ systemNanoseconds systemTime) <>
        "message" =: msgText
      botParams =
        if isMsgTextRepeatCommand msgText'
          then botParams' <> "keyboard" =: keyboard
          else botParams'
   in responseBody <$> makeRequest urlScheme botParams
