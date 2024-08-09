{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Layer.Writer ()
where

-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (MonadTrans, lift)

-- typeclasses----------------------------------------------------------------
import           Control.Monad.Writer.Class

------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad (t m), MonadWriter w m) =>
    MonadWriter w (t m)
  where
    writer = lift . writer
    {-# INLINABLE writer #-}
    tell = lift . tell
    {-# INLINABLE tell #-}
    listen m = m >>= lift . listen . return
    {-# INLINABLE listen #-}
    pass m = m >>= lift . pass . return
    {-# INLINABLE pass #-}
