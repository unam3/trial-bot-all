module Main where

import System.Log.Logger
  ( Priority(DEBUG, ERROR)
  , debugM
  , errorM
  , infoM
  , setLevel
  , traplogging
  , updateGlobalLogger
  )

import qualified Config
import qualified Tg
import qualified Vk
import qualified Logger as L

main :: IO ()
main = 
  L.withLogger
    (L.Config
       -- use INFO, DEBUG or ERROR here
       -- (add to System.Log.Logger import items if missed)
       DEBUG
       (traplogging "trial-bot-all" ERROR "Unhandled exception occured" .
        updateGlobalLogger "trial-bot-all" . setLevel)
       (debugM "trial-bot-all")
       (infoM "trial-bot-all")
       (errorM "trial-bot-all"))
    (\ loggerH -> -- startBot loggerH args)
        Config.parseConfig
            >>= \ eitherConfig -> case eitherConfig of
                Left errorMessage -> L.hError loggerH errorMessage
                Right config -> case Config.c_run config of
                    Config.Tg -> case Config.c_tg config of
                        Just tgConfig -> Tg.startBot loggerH tgConfig
                        _ -> L.hError loggerH "Telegram parameters must be configured."
                    Config.Vk -> case Config.c_vk config of
                        Just vkConfig -> Vk.startBot loggerH vkConfig
                        _ -> L.hError loggerH "Vkotakter parameters must be configured."

        )
