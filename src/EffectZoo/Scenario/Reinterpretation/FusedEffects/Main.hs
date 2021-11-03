module EffectZoo.Scenario.Reinterpretation.FusedEffects.Main where

import           Control.Monad
import           Data.Function
import           EffectZoo.Scenario.Reinterpretation.FusedEffects.HTTP
import           EffectZoo.Scenario.Reinterpretation.FusedEffects.Logging
import           EffectZoo.Scenario.Reinterpretation.FusedEffects.Zooit   as Zooit
import           EffectZoo.Scenario.Reinterpretation.Shared

listScenarios :: Int -> IO ([String], [String])
listScenarios n =
  fmap concat (replicateM n Zooit.listScenarios)
    & toLoggedHTTP
    & mockResponses response
    & accumulateLogMessages
