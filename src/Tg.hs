{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg
  ( getInt
  , getLatestSupportedUpdateContent
  , processArgs
  , startBotWithLogger
  ) where

import Control.Monad (replicateM_, void)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, append, pack)
import Data.Text.Read (decimal)
import Prelude hiding (id)
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

import qualified Tg.Logger as L
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

processArgs :: [String] -> Either String Config
processArgs [token, helpMsg, repeatMsg, echoRepeatNumberStr] =
  let echoRepeatNumber = (read echoRepeatNumberStr :: Int)
      isInRange n = n > 0 && n < 6
   in if or
           [ null token
           , null helpMsg
           , null repeatMsg
           , not $ isInRange echoRepeatNumber
           ]
        then Left "Some argument passed from command line is wrong."
        else Right
               Config
                 { tokenSection = append "bot" $ pack token
                 , helpMessage = pack helpMsg
                 , repeatMessage = pack repeatMsg
                 , numberOfRepeats = pack echoRepeatNumberStr
                 , numberOfRepeatsMap = M.empty
                 }
processArgs _ =
  Left
    "Exactly four arguments needed: token, helpMsg, repeatMsg, echoRepeatNumber."

startBot :: L.Handle () -> [String] -> IO ()
startBot loggerH args =
  case processArgs args of
    Right config ->
      L.hInfo loggerH "Bot is up and running." >> cycleEcho loggerH config >>
      exitSuccess
    Left errorMessage -> L.hError loggerH errorMessage >> exitFailure

startBotWithLogger :: [String] -> IO ()
startBotWithLogger args = do
  L.withLogger
    (L.Config
                -- use INFO, DEBUG or ERROR here
                -- (add to System.Log.Logger import items if missed)
       DEBUG
       (traplogging "trial-bot" ERROR "Unhandled exception occured" .
        updateGlobalLogger "trial-bot" . setLevel)
       (debugM "trial-bot")
       (infoM "trial-bot")
       (errorM "trial-bot"))
    (\loggerH -> startBot loggerH args)
  pure ()
