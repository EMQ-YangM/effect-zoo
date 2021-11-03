{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module EffectZoo.Scenario.BigStack.FusedEffects.Identity where

import           Control.Algebra
import           Control.Monad.IO.Class
import           Data.Kind

data Identity (m :: Type -> Type) a where
  Noop :: Identity m ()

newtype IdentityC m a = IdentityC { runIdentityC :: m a }
  deriving (Functor , Applicative , Monad , MonadIO)

instance Algebra sig m => Algebra (Identity :+: sig) (IdentityC m) where
  alg hdl sig ctx = IdentityC $ case sig of
    L Noop  -> pure ctx
    R other -> alg (runIdentityC . hdl) other ctx
  {-# INLINE alg #-}


runIdentity :: IdentityC m a -> m a
runIdentity  = runIdentityC
{-# INLINE runIdentity #-}
