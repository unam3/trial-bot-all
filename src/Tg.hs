{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg
  ( getInt
  , getSupportedUpdatesContent
  , processConfig
  , startBot
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO, try)
import Control.Monad (replicateM_, void)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text, append, pack)
import Data.Text.Read (decimal)
import Network.HTTP.Req (HttpException(VanillaHttpException))
import Prelude hiding (id)
import System.Exit (exitFailure, exitSuccess)

import Config (TgConfig(..))
import qualified Logger as L
import Tg.Requests
import Tg.Requests.JSON
import Tg.Types

getLatestUpdateId :: ResponseJSON -> Maybe Offset
getLatestUpdateId rjson =
  let updates = result rjson
   in if null updates
        then Nothing
        else Just (update_id $ last updates)

getInt :: Text -> Int
getInt = fst . fromRight (1, "1") . decimal

-- -> EitherUdateContent
type MaybeUpdateContent
   = Maybe (Either (ChatID, Text, Username, UserID) CallbackQuery)

makeMaybeUpdateContent :: Maybe Message -> Maybe Text-> Maybe User -> Maybe Username -> MaybeUpdateContent
makeMaybeUpdateContent (Just message') (Just text') (Just user') (Just username') =
    let chatID = id $ (chat :: Message -> Chat) message'
        userID = (_id :: User -> UserID) user'
    in Just $ Left (chatID, text', username', userID)
makeMaybeUpdateContent _ _ _ _ = Nothing

getSupportedUpdatesContent :: [Update] -> [MaybeUpdateContent]
getSupportedUpdatesContent (update:updateList) =
  let maybeMessage = message update
      maybeText = maybeMessage >>= (text :: Message -> Maybe Text)
      maybeUser = maybeMessage >>= from
      maybeUsername = maybeUser >>= _username
      maybeUpdateContent = makeMaybeUpdateContent maybeMessage maybeText maybeUser maybeUsername
      maybeCallbackQuery = callback_query update
   in if isJust maybeUpdateContent
        then maybeUpdateContent : getSupportedUpdatesContent updateList
        else if isJust maybeCallbackQuery
            then fmap Right maybeCallbackQuery : getSupportedUpdatesContent updateList
            else getSupportedUpdatesContent updateList
getSupportedUpdatesContent [] = []

isHelpCommand :: Text -> Bool
isHelpCommand = (== "/help")

getNumberOfRepeats :: Text -> Text
getNumberOfRepeats "repeat5" = "5"
getNumberOfRepeats "repeat4" = "4"
getNumberOfRepeats "repeat3" = "3"
getNumberOfRepeats "repeat2" = "2"
getNumberOfRepeats _ = "1"

processUpdates :: IO BotParams -> MaybeUpdateContent -> IO BotParams
processUpdates ioBotParams maybeUpdateContent =
  do
  botParams <- ioBotParams
  case maybeUpdateContent of
    Just (Left (chatID, msg, username, userID)) ->
      let numberOfRepeats' =
            if isRepeatCommand msg || isHelpCommand msg
              then 1
              else getInt $
                   M.findWithDefault
                     (defaultNumberOfRepeatsText $ config botParams)
                     userID
                     (numberOfRepeatsMap botParams)
       in replicateM_
            numberOfRepeats'
            (respondToMessage botParams chatID msg username userID) >>
          return botParams
        -- https://core.telegram.org/bots/api#answercallbackquery
    Just (Right callbackQuery) ->
      void (answerCallbackQuery (tokenSection $ config botParams) callbackQuery) >>
      let userID = (_id :: User -> UserID) $ _from callbackQuery
          newNumberOfRepeats = getNumberOfRepeats $ _data callbackQuery
          newNumberOfRepeatsMap =
            M.insert userID newNumberOfRepeats (numberOfRepeatsMap botParams)
       in return botParams {numberOfRepeatsMap = newNumberOfRepeatsMap}
    _ -> return botParams

processResponse :: BotParams -> ResponseJSON -> IO BotParams
processResponse botParams rjson =
    let updates = result rjson
        supportedUpdatesContent = getSupportedUpdatesContent updates
    in foldl processUpdates (pure botParams) supportedUpdatesContent

cycleEcho' :: L.Handle () -> BotParams -> Maybe ResponseJSON -> IO ResponseJSON
cycleEcho' loggerH botParams maybeRJSON =
  let maybeOffset = maybeRJSON >>= getLatestUpdateId
   in getUpdates (tokenSection $ config botParams) maybeOffset >>= \ioRJSON ->
        L.hDebug loggerH (show ioRJSON) >> processResponse botParams ioRJSON >>= \botParams' ->
          cycleEcho' loggerH botParams' $ Just ioRJSON

cycleEcho :: L.Handle () -> BotParams -> IO ResponseJSON
cycleEcho loggerH botParams =
  let noRJSON = Nothing
   in cycleEcho' loggerH botParams noRJSON

guardedCycleEcho :: L.Handle () -> BotParams -> IO ResponseJSON
guardedCycleEcho loggerH botParams =
    do
    eitherReqError <- try (cycleEcho loggerH botParams)
    case eitherReqError of
        Left (VanillaHttpException e) ->
            L.hError loggerH "Seems like network is down. Trying another request in 2 seconds."
                >> L.hDebug loggerH (show e)
                    >> threadDelay 2000000
                        >> guardedCycleEcho loggerH botParams
        Left jsonHttpException -> throwIO jsonHttpException
        Right _ -> exitSuccess


processConfig :: TgConfig -> Either String BotParams
processConfig (TgConfig token helpMsg repeatMsg echoRepeatNumber) =
  let isInRange n = n > 0 && n < 6
   in if not $ isInRange echoRepeatNumber
        then Left
               "Number of message repeats (echoRepeatNumber) must be 1, 2, 3, 4 or 5."
        else Right
               BotParams {
                config = Config
                 { tokenSection = append "bot" token
                 , helpMessage = helpMsg
                 , repeatMessage = repeatMsg
                 , defaultNumberOfRepeatsText = pack $ show echoRepeatNumber
                 },
                numberOfRepeatsMap = M.empty
               }


startBot :: L.Handle () -> TgConfig -> IO ()
startBot loggerH parsedConfig =
  case processConfig parsedConfig of
    Right botParams ->
      L.hInfo loggerH "Telegram bot is up and running."
        >> void (guardedCycleEcho loggerH botParams)
    Left errorMessage -> L.hError loggerH errorMessage >> exitFailure
