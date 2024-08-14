{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Layer.Error ()
where

import Control.Monad.Lift

import Control.Monad.Except

instance {-# OVERLAPPABLE #-}
    (MonadTransControl t, MonadError e m, Monad (t m))
  => MonadError e (t m) where
   
   throwError = lift . throwError
   catchError m h = control (\peel -> catchError (peel m) (peel . h))
