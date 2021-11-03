{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module EffectZoo.Scenario.Reinterpretation.FusedEffects.Zooit where

import           Control.Algebra
import           Control.Monad.IO.Class
import           Data.Kind
import           EffectZoo.Scenario.Reinterpretation.FusedEffects.HTTP
import           EffectZoo.Scenario.Reinterpretation.FusedEffects.Logging

data Zooit (m :: Type -> Type) a where
  ListScenarios :: Zooit m [String]

listScenarios :: Has Zooit sig m  => m [String]
listScenarios = send ListScenarios
{-# INLINE listScenarios #-}

newtype LoggedHTTPC m a = LoggedHTTPC { runLoggedHTTPC :: m a }
  deriving (Functor , Applicative , Monad, MonadIO)

instance Has (HTTP :+: Logging) sig m => Algebra (Zooit :+: sig) (LoggedHTTPC m) where
  alg hdl sig ctx = LoggedHTTPC $ case sig of
    (L ListScenarios) -> do
      logMsg "Fetching a list of scenarios"
      res <- lines <$> httpGET "/scenarios"
      pure (res <$ ctx)
    R other -> alg (runLoggedHTTPC . hdl) other ctx
  {-# INLINE alg #-}

toLoggedHTTP :: LoggedHTTPC m a -> m a
toLoggedHTTP  = runLoggedHTTPC
{-# INLINE toLoggedHTTP #-}
