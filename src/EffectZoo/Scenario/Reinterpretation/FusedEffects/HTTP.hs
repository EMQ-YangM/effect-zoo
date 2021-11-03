{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module EffectZoo.Scenario.Reinterpretation.FusedEffects.HTTP where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Monad.IO.Class
import           Data.Kind

data HTTP (m :: Type -> Type) a where
  GET :: String -> HTTP m String

httpGET :: Has HTTP sig m =>String -> m String
httpGET = send . GET
{-# INLINE httpGET #-}

newtype ReaderHTTPC m a = ReaderHTTPC { runReaderHTTPC :: (ReaderC String m) a }
  deriving (Functor , Applicative , Monad, MonadIO)

instance Algebra sig m => Algebra (HTTP :+: sig) (ReaderHTTPC m) where
  alg hdl sig ctx = ReaderHTTPC $ case sig of
    L (GET s) -> ReaderC $ \r -> pure (r <$ ctx)
    R other   -> alg (runReaderHTTPC . hdl) (R other) ctx
  {-# INLINE alg #-}

mockResponses :: String -> ReaderHTTPC m a -> m a
mockResponses s f = runReader s $ runReaderHTTPC f
{-# INLINE mockResponses #-}
