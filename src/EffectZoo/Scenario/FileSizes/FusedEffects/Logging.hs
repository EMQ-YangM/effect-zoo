{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module EffectZoo.Scenario.FileSizes.FusedEffects.Logging where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Kind
import qualified EffectZoo.Scenario.FileSizes.Shared as Shared

data Logging (m :: Type -> Type) k where
  LogMsg :: String -> Logging m ()

logMsg :: Has Logging sig m => String -> m ()
logMsg st = send (LogMsg st)
{-# INLINE logMsg #-}

newtype LogIOC m a = LogIOC
  { unLogIOC :: ReaderC (IORef [String]) m a
  } deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Logging :+: sig) (LogIOC m) where
  alg hdl sig ctx = LogIOC $ case sig of
    L (LogMsg s) -> ReaderC $ \ref -> liftIO (Shared.logToIORef ref s) >> pure ctx
    R other -> alg (unLogIOC . hdl) (R other) ctx
  {-# INLINE alg #-}

runLogIOC :: IORef [String] -> LogIOC m a -> m a
runLogIOC r f = runReader r $ unLogIOC f
{-# INLINE runLogIOC #-}
