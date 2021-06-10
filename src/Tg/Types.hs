module Tg.Types where

import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)

type Offset = Int32

type ChatID = Int64

type Username = Text

type TokenSection = Text

type HelpMessage = Text

type RepeatMessage = Text

type UserID = Int64

type NumberOfRepeats = Text

type NumberOfRepeatsMap = Map UserID NumberOfRepeats

data Config =
  Config
    { tokenSection :: TokenSection
    , helpMessage :: HelpMessage
    , repeatMessage :: RepeatMessage
    , numberOfRepeats :: NumberOfRepeats
    , numberOfRepeatsMap :: NumberOfRepeatsMap
    }
  deriving (Show, Eq)
