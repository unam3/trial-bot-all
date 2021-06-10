module Main where

import System.Environment (getArgs)
import qualified Tg
import qualified Vk

main :: IO ()
main = getArgs
    >>= \ args ->
        let botToRun = if length args == 4
            then Tg.startBotWithLogger
            else Vk.startBotWithLogger
        in botToRun args
