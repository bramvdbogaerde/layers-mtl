{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Layer.Reader() where

import Control.Monad.Lift
import Control.Monad.Reader.Class

instance {-# OVERLAPPABLE #-}
    (MonadTrans t, MInvariant t, MonadReader r m, Monad (t m))
  =>
    MonadReader r (t m)
  where
    reader = lift . reader
    {-# INLINABLE reader #-}
    ask = lift ask
    {-# INLINABLE ask #-}
    local f m = lift ask >>= \r -> hoistiso (local f) (local (const r)) m
    {-# INLINABLE local #-}
