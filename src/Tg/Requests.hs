{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.Requests
  ( answerCallbackQuery
  , getUpdates
  , isRepeatCommand
  , commandOrText
  , respondToMessage
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)
import Data.Map.Strict (findWithDefault)
import Data.Text (Text)
import Network.HTTP.Req

import Tg.Requests.JSON
import Tg.Types

makeRequest ::
     (HttpBody body, MonadIO m, FromJSON a)
  => Url scheme
  -> body
  -> m (JsonResponse a)
makeRequest urlScheme body =
  runReq defaultHttpConfig $ req POST urlScheme body jsonResponse mempty

withIncrementedUpdatesOffset :: Offset -> RequestJSON
withIncrementedUpdatesOffset updatesOffset =
  WithOffset {timeout = 20, offset = updatesOffset + 1}

getUpdates :: TokenSection -> Maybe Offset -> IO ResponseJSON
getUpdates tokenSection' maybeOffset =
  let apiMethod = "getUpdates"
      urlScheme = https "api.telegram.org" /: tokenSection' /: apiMethod
      body =
        ReqBodyJson $
        maybe
          (WithoutOffset {timeout = 20})
          withIncrementedUpdatesOffset
          maybeOffset
   in responseBody <$> makeRequest urlScheme body

isRepeatCommand :: Text -> Bool
isRepeatCommand = (== "/repeat")

commandOrText :: BotParams -> Username -> UserID -> Text -> Text
commandOrText botParams _ _ "/help" = helpMessage $ config botParams
commandOrText botParams username userID "/repeat" =
  let echoRepeatNumber =
        findWithDefault
          (defaultNumberOfRepeatsText $ config botParams)
          userID
          (numberOfRepeatsMap botParams)
   in mconcat
        [ "@"
        , username
        , " Current number of repeats is "
        , echoRepeatNumber
        , ". "
        , repeatMessage $ config botParams
        ]
commandOrText _ _ _ t = t

respondToMessage ::
     BotParams -> ChatID -> Text -> Username -> UserID -> IO ResponseStatusJSON
respondToMessage botParams chatID msg username userID =
  let apiMethod = "sendMessage"
      urlScheme =
        https "api.telegram.org" /: (tokenSection $ config botParams) /:
        apiMethod
      request =
        EchoRequest
          { text = commandOrText botParams username userID msg
          , chat_id = chatID
          , reply_markup =
              if isRepeatCommand msg
                then let buttons =
                           [ [ InlineKeyboardButton
                                 {text = "1", callback_data = "repeat1"}
                             , InlineKeyboardButton
                                 {text = "2", callback_data = "repeat2"}
                             , InlineKeyboardButton
                                 {text = "3", callback_data = "repeat3"}
                             , InlineKeyboardButton
                                 {text = "4", callback_data = "repeat4"}
                             , InlineKeyboardButton
                                 {text = "5", callback_data = "repeat5"}
                             ]
                           ]
                      in Just InlineKeyboardMarkup {inline_keyboard = buttons}
                else Nothing
          }
      body = ReqBodyJson request
   in responseBody <$> makeRequest urlScheme body

answerCallbackQuery :: TokenSection -> CallbackQuery -> IO ResponseStatusJSON
answerCallbackQuery tokenSection' callbackQuery =
  let apiMethod = "answerCallbackQuery"
      urlScheme = https "api.telegram.org" /: tokenSection' /: apiMethod
      body =
        ReqBodyJson $
        AnswerCallbackRequest
          {callback_query_id = (_id :: CallbackQuery -> Text) callbackQuery}
   in responseBody <$> makeRequest urlScheme body
