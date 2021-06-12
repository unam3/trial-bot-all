{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg
  ( getInt
  , getLatestSupportedUpdateContent
  , processArgs
  , startBot
  ) where

import Control.Monad (replicateM_, void)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, append, pack)
import Data.Text.Read (decimal)
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

type MaybeUpdateContent
   = Maybe (Either (ChatID, Text, Username, UserID) CallbackQuery)

getLatestSupportedUpdateContent' :: [Update] -> MaybeUpdateContent
getLatestSupportedUpdateContent' (update:updateList) =
  let maybeMessage = message update
      chatID = id . (chat :: Message -> Chat) $ fromJust maybeMessage
      maybeText = maybeMessage >>= (text :: Message -> Maybe Text)
      maybeUser = maybeMessage >>= from
      maybeUsername = maybeUser >>= _username
      userID = (_id :: User -> UserID) $ fromJust maybeUser
      maybeCallbackQuery = callback_query update
   in if isJust maybeText && isJust maybeUsername
        then Just $
             Left (chatID, fromJust maybeText, fromJust maybeUsername, userID)
        else maybe
               (getLatestSupportedUpdateContent' updateList)
               (Just . Right)
               maybeCallbackQuery
getLatestSupportedUpdateContent' [] = Nothing

getLatestSupportedUpdateContent :: ResponseJSON -> MaybeUpdateContent
getLatestSupportedUpdateContent rjson =
  let updates = result rjson
   in getLatestSupportedUpdateContent' $ reverse updates

isHelpCommand :: Text -> Bool
isHelpCommand = (== "/help")

getNumberOfRepeats :: Text -> Text
getNumberOfRepeats "repeat5" = "5"
getNumberOfRepeats "repeat4" = "4"
getNumberOfRepeats "repeat3" = "3"
getNumberOfRepeats "repeat2" = "2"
getNumberOfRepeats _ = "1"

processUpdates :: Config -> ResponseJSON -> IO Config
processUpdates config ioRJSON =
  case getLatestSupportedUpdateContent ioRJSON of
    Just (Left (chatID, msg, username, userID)) ->
      let numberOfRepeats' =
            if isRepeatCommand msg || isHelpCommand msg
              then 1
              else getInt $
                   M.findWithDefault
                     (numberOfRepeats config)
                     userID
                     (numberOfRepeatsMap config)
       in replicateM_
            numberOfRepeats'
            (respondToMessage config chatID msg username userID) >>
          return config
        -- https://core.telegram.org/bots/api#answercallbackquery
    Just (Right callbackQuery) ->
      void (answerCallbackQuery (tokenSection config) callbackQuery) >>
      let userID = (_id :: User -> UserID) $ _from callbackQuery
          newNumberOfRepeats = getNumberOfRepeats $ _data callbackQuery
          newNumberOfRepeatsMap =
            M.insert userID newNumberOfRepeats (numberOfRepeatsMap config)
       in return config {numberOfRepeatsMap = newNumberOfRepeatsMap}
    _ -> return config

cycleEcho' :: L.Handle () -> Config -> Maybe ResponseJSON -> IO ResponseJSON
cycleEcho' loggerH config maybeRJSON =
  let maybeOffset = maybeRJSON >>= getLatestUpdateId
   in getUpdates (tokenSection config) maybeOffset >>= \ioRJSON ->
        L.hDebug loggerH (show ioRJSON) >> processUpdates config ioRJSON >>= \config' ->
          cycleEcho' loggerH config' $ Just ioRJSON

cycleEcho :: L.Handle () -> Config -> IO ResponseJSON
cycleEcho loggerH config =
  let noRJSON = Nothing
   in cycleEcho' loggerH config noRJSON

processArgs :: TgConfig -> Either String Config
processArgs (TgConfig token helpMsg repeatMsg echoRepeatNumber) =
  let isInRange n = n > 0 && n < 6
   in if not $ isInRange echoRepeatNumber
        then Left
               "Number of message repeats (echoRepeatNumber) must be 1, 2, 3, 4 or 5."
        else Right
               Config
                 { tokenSection = append "bot" token
                 , helpMessage = helpMsg
                 , repeatMessage = repeatMsg
                 , numberOfRepeats = pack $ show echoRepeatNumber
                 , numberOfRepeatsMap = M.empty
                 }

startBot :: L.Handle () -> TgConfig -> IO ()
startBot loggerH parsedConfig =
  case processArgs parsedConfig of
    Right config ->
      L.hInfo loggerH "Telegram bot is up and running." >>
      cycleEcho loggerH config >>
      exitSuccess
    Left errorMessage -> L.hError loggerH errorMessage >> exitFailure
