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

module EffectZoo.Scenario.FileSizes.FusedEffects.File where

import           Control.Algebra
import           Control.Monad.IO.Class
import           Data.Kind
import           EffectZoo.Scenario.FileSizes.FreerSimple.File (tryFileSize)
import qualified EffectZoo.Scenario.FileSizes.Shared           as Shard
import qualified EffectZoo.Scenario.FileSizes.Shared           as Shared

data File (m :: Type -> Type) a where
  TryFileSize :: FilePath -> File m (Maybe Int)

tryFileSize :: Has File sig m => FilePath -> m (Maybe Int)
tryFileSize fp = send (TryFileSize fp )
{-# INLINE tryFileSize #-}

newtype FileIOC m a = FileIOC
  { runFileIOC :: m a
  } deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (File :+: sig) (FileIOC m) where
  alg hdl sig ctx = FileIOC $ case sig of
    L (TryFileSize fp) -> (<$ ctx) <$> liftIO (Shard.tryGetFileSize fp)
    R other            -> alg (runFileIOC . hdl) other ctx
  {-# INLINE alg #-}

runFileIOC2 :: FileIOC m a -> m a
runFileIOC2 = runFileIOC
{-# INLINE runFileIOC2 #-}
