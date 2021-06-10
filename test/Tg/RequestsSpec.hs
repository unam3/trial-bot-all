{-# LANGUAGE OverloadedStrings #-}

module Tg.RequestsSpec where

import qualified Data.Map.Strict as M
import Test.Hspec

import Tg.Requests (commandOrText)
import Tg.Types (Config(..))

testConfig :: Config
testConfig =
  Config
    { tokenSection = "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11"
    , helpMessage = "helpMessage"
    , repeatMessage = "repeatMessage"
    , numberOfRepeats = "1"
    , numberOfRepeatsMap = M.empty
    }

spec :: Spec
spec =
  describe "commandOrText" $ do
    it "returns help message" $
      shouldBe
        (commandOrText testConfig "TESTUSERNAME" 123 "/help")
        (helpMessage testConfig)
    it "returns repeat message" $
      shouldBe
        (commandOrText testConfig "TESTUSERNAME" 123 "/repeat")
        "@TESTUSERNAME Current number of repeats is 1. repeatMessage"
    it "returns any other message as is" $
      shouldBe
        (commandOrText testConfig "TESTUSERNAME" 123 "/help as is")
        "/help as is"
