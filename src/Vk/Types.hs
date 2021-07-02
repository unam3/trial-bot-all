module Vk.Types where

import Data.Map.Strict (Map)
import Data.Text (Text)

type TokenSection = Text

type GroupId = Text

type HelpMessage = Text

type RepeatMessage = Text

type DefaultNumberOfRepeatsText = Text

type FromId = Int

type NumberOfRepeatsMap = Map FromId DefaultNumberOfRepeatsText

type Config
   = ( TokenSection
     , GroupId
     , HelpMessage
     , RepeatMessage
     , DefaultNumberOfRepeatsText)

type BotParams = (Config, NumberOfRepeatsMap)
