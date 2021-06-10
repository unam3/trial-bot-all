module Vk.Types where

import Data.Map.Strict (Map)
import Data.Text (Text)

type TokenSection = Text

type GroupId = Text

type HelpMessage = Text

type RepeatMessage = Text

type NumberOfRepeats = Text

type FromId = Int

type NumberOfRepeatsMap = Map FromId NumberOfRepeats

type Config
   = ( TokenSection
     , GroupId
     , HelpMessage
     , RepeatMessage
     , NumberOfRepeats
     , NumberOfRepeatsMap)
