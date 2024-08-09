{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module defines an overlappable 'MonadTrans' instance for the 'MonadState' @(monadinterface, interface). 
module Control.Monad.Layer.State() where

-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (MonadTrans, lift)


-- monad interaces -----------------------------------------------------------

import           Control.Monad.State.Class

------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadState s m, Monad (t m)) =>
    MonadState s (t m)
  where
    state = lift . state
    {-# INLINABLE state #-}
    get = lift get
    {-# INLINABLE get #-}
    put = lift . put
    {-# INLINABLE put #-}
