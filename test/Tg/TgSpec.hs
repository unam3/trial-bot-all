{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.TgSpec where

import Data.Either (isRight)
import Data.Text (Text)
import Prelude hiding (id)
import Test.Hspec

import Tg (getInt, getLatestSupportedUpdateContent, processArgs)
import Tg.Requests.JSON

spec :: Spec
spec = do
  describe "processArgs" $ do
    it "returns config" $
      shouldSatisfy
        (processArgs
           [ "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11"
           , "help msg"
           , "repeat msg"
           , "1"
           ])
        isRight
    it "returns error if wrong number of arguments passed" $
      shouldBe
        (processArgs ["pluh"])
        (Left
           "Exactly four arguments needed: token, helpMsg, repeatMsg, echoRepeatNumber.")
    it "returns error if empty token" $
      shouldBe
        (processArgs ["", "help msg", "repeat msg", "1"])
        (Left "Some argument passed from command line is wrong.")
    it "returns error if empty help message" $
      shouldBe
        (processArgs
           ["123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11", "", "repeat msg", "1"])
        (Left "Some argument passed from command line is wrong.")
    it "returns error if empty repeat message" $
      shouldBe
        (processArgs
           ["123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11", "help msg", "", "1"])
        (Left "Some argument passed from command line is wrong.")
    it "returns error if wrong number of repeats" $
      shouldBe
        (processArgs
           [ "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11"
           , "help msg"
           , "repeat msg"
           , "0"
           ])
        (Left "Some argument passed from command line is wrong.")
  describe "getInt" . it "returns Int" $
    getInt ("5" :: Text) `shouldBe` (5 :: Int)
  describe "getLatestSupportedUpdateContent" $ do
    let responseWithoutUpdates = (RJSON {ok = True, result = []})
    it "returns Nothing if no updates" $
      getLatestSupportedUpdateContent responseWithoutUpdates `shouldBe` Nothing
    let responseWithUpdates =
          RJSON
            { ok = True
            , result =
                [ Update
                    { update_id = 858301205
                    , message =
                        Just
                          (Message
                             { chat = Chat {id = 123456789}
                             , from =
                                 Just
                                   (User {_username = Just "A", _id = 111111111})
                             , text = Just "44"
                             })
                    , callback_query = Nothing
                    }
                , Update
                    { update_id = 858301206
                    , message =
                        Just
                          (Message
                             { chat = Chat {id = 123456789}
                             , from =
                                 Just
                                   (User {_username = Just "B", _id = 222222222})
                             , text = Just "11"
                             })
                    , callback_query = Nothing
                    }
                -- unsupported update
                , Update
                    { update_id = 858301207
                    , message =
                        Just
                          (Message
                             { chat = Chat {id = 123456789}
                             , from =
                                 Just
                                   (User {_username = Just "C", _id = 333333333})
                             , text = Nothing
                             })
                    , callback_query = Nothing
                    }
                ]
            }
        latestSupportedUpdate = Just (Left (123456789, "11", "B", 222222222))
    it "returns latest supported update" $
      getLatestSupportedUpdateContent responseWithUpdates `shouldBe`
      latestSupportedUpdate
