module EffectZoo.Scenario.CountDown.FusedEffects.Main where

import           Control.Carrier.State.Strict
import           EffectZoo.Scenario.CountDown.FusedEffects.Program

countDown :: Int -> (Int, Int)
countDown initial = run (runState initial program)
