{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module EffectZoo.Scenario.BigStack.FusedEffects.Program where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Monad

program
  :: Has (State Int :+: Reader Int) si m
  => m ()
program = do
  n <- ask
  replicateM_ n (modify (+ n))
