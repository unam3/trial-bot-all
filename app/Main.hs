module Main where

import System.Environment (getArgs)
import Vk (startBotWithLogger)

main :: IO ()
main = getArgs >>= startBotWithLogger
