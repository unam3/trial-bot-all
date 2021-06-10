module Main where

import System.Environment (getArgs)
import Tg (startBotWithLogger)

main :: IO ()
main = getArgs >>= startBotWithLogger
