{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module EffectZoo.Scenario.Reinterpretation.FusedEffects.Logging where

-- import           Data.Coerce
-- import           Control.Effect
-- import           Control.Effect.Carrier
-- import           Control.Effect.Sum
-- import           Control.Effect.Writer
import           Control.Algebra
import           Control.Carrier.Writer.Strict
import           Control.Monad.IO.Class
import           Data.Kind


data Logging (m :: Type -> Type) a where
  LogMsg :: String -> Logging m ()

logMsg :: Has Logging sig m => String -> m ()
logMsg msg = send (LogMsg msg)
{-# INLINE logMsg #-}

newtype WriterLoggingC m a = WriterLoggingC { runWriterLoggingC :: (WriterC [String] m) a }
  deriving (Functor, Applicative , Monad , MonadIO)

instance Algebra sig m => Algebra (Logging :+: sig) (WriterLoggingC m) where
  alg hdl sig ctx = WriterLoggingC $ case sig of
    L (LogMsg s) -> tell [s] >> pure ctx
    R other      -> alg (runWriterLoggingC . hdl) (R other) ctx
  {-# INLINE alg #-}

accumulateLogMessages :: WriterLoggingC m a -> m ([String], a)
accumulateLogMessages  = runWriter . runWriterLoggingC
{-# INLINE accumulateLogMessages #-}
