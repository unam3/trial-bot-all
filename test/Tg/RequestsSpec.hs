{-# LANGUAGE OverloadedStrings #-}

module Tg.RequestsSpec where

import qualified Data.Map.Strict as M
import Test.Hspec

import Tg.Requests (commandOrText)
import Tg.Types (BotParams(..), Config(..))

testBotParams :: BotParams
testBotParams =
  BotParams {
    config = Config { tokenSection = "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11"
    , helpMessage = "helpMessage"
    , repeatMessage = "repeatMessage"
    , defaultNumberOfRepeatsText = "1"
    }
    , numberOfRepeatsMap = M.empty
  }

spec :: Spec
spec =
  describe "commandOrText" $ do
    it "returns help message" $
      shouldBe
        (commandOrText testBotParams "TESTUSERNAME" 123 "/help")
        (helpMessage $ config testBotParams)
    it "returns repeat message" $
      shouldBe
        (commandOrText testBotParams "TESTUSERNAME" 123 "/repeat")
        "@TESTUSERNAME Current number of repeats is 1. repeatMessage"
    it "returns any other message as is" $
      shouldBe
        (commandOrText testBotParams "TESTUSERNAME" 123 "/help as is")
        "/help as is"
