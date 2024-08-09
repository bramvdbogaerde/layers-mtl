{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Layer.Cont ()
where

-- mtl -----------------------------------------------------------------------

import           Control.Monad.Cont.Class

-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                    ( MonadTransControl
                    , lift, liftControl, resume
                    )

------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (MonadTransControl t, MonadCont m, Monad (t m)) =>
    MonadCont (t m)
  where
    callCC f = liftControl (\peel -> callCC $ \c -> peel . f $ \a ->
        lift (peel (return a) >>= c)) >>= resume
    {-# INLINABLE callCC #-}
