module Control.Monad.Lift.Class(MonadTransControl(..), LayerEffects) where

-- transformers --------------------------------------------------------------

import           Control.Monad.Trans.Class (MonadTrans (lift))

-- base ---------------------------------------------------------------------

import           Data.Kind
import           Data.Proxy

-- auxilariy --------------------------------------------------------------- 

type TransformerKind = (Type -> Type) -> Type -> Type
type MonadKind       = Type -> Type

------------------------------------------------------------------------------
-- | The constraint @'MonadTransControl' t@ holds if @t@ is a
-- G(monadtransformer,monad transformer) through which
-- G(controloperation,control operations) can be lifted. There are a
-- variety of operations for doing so, depending on the exact type of the
-- G(controloperation,control operation) in question, including
-- 'liftControl', 'control', 'liftOp', 'liftOp_' and 'liftDiscard'. These are
-- all built on top of the more primitive 'capture', 'suspend' and 'resume'
-- operations.
--
-- The thing that all \"control\" operations have in common is that they have
-- a monadic argument somewhere in a contravariant position (otherwise they
-- could be lifted with just 'lift'). For example, let's say you have a
-- control operation @f :: m a -> m b@, and you want to make a lifted version
-- @f' :: t m a -> t m b@. @f'@ needs to somehow \"lower\" its argument from
-- @t m a@ to @m a@ so that it can be passed to the original @f@.
--
-- Naively, we might try to type class an operation like this and call it
-- @peel@ (because it \"peels\" off the @t@ layer of a @t m a@ computation):
--
-- @peel :: 'Monad' m => t m a -> m a@
--
-- Unfortunately, the only monad transformers that could provide such an
-- operation are trivial (i.e., isomorphic to 'IdentityT'). To see why other
-- monad transformers cannot provide such an operation, let's consider the
-- more complicated 'RWST' monad transformer. It is defined like this:
--
-- @newtype 'RWST' r w s m a = 'RWST' { 'Control.Monad.Trans.RWS.Strict.runRWST' :: r -> s -> m (a, s, w) }@
--
-- It's not possible to make an operation @'RWST' r w s m a -> m a@, because
-- 'RWST'​'s inner function needs an @r@ and an @s@ value before it can return
-- a value in @m@. However, we could write an operation
-- @'RWST' r w s m a -> r -> s -> m a@. Given that we have also have the
-- operation @'IdentityT' m a -> m a@ for 'IdentityT', we want some pattern
-- that can abstract this into a single operation. To do this we introduce an
-- associated type synonym to the 'MonadTransControl' class called
-- 'LayerState'.
-- 
-- @type 'LayerState' 'IdentityT' = ()
--type 'LayerState' ('RWST' r w s) = (r, s)@
--
-- Now we can have an operation with a single type that fits both 'RWST' and
-- 'IdentityT':
--
-- @peel :: 'Monad' m => t m a -> 'LayerState' t -> m a@
--
-- This is better, but it's still far from perfect. First of all, we don't
-- really want to peel away the @t@ layer completely, because then we lose all
-- of its side-effects. What we really want is to be able to 'suspend' them
-- temporarily (allowing us to work in the monad @m@ underneath @t@) and then
-- later 'resume' them when we're finished working in @m@. Secondly, even if
-- peeling really was what we wanted to do, there are several monad
-- transformers for which we couldn't do this even if we wanted to. Consider
-- 'MaybeT', defined as follows:
--
-- @newtype 'MaybeT' m a = { 'Control.Monad.Trans.Maybe.runMaybeT' :: m ('Maybe' a) }@
-- 
-- If the value inside the @m@ computation wrapped by 'MaybeT' is a 'Nothing',
-- we can't get an @a@ out of it. The closest to the proposed type for 'peel'
-- above that we could get for 'MaybeT' would be:
--
-- @peel :: 'MaybeT' m a -> 'LayerState' 'MaybeT' -> m ('Maybe' a)@
--
-- Similarly, for @'ExceptT' e@, the closest we could get would be:
--
-- @peel :: 'ExceptT' e m a -> 'LayerState' ('ExceptT' e) -> m ('Either' e a)@
--
-- Again, we can use an associated type synonym to make all of these
-- operations fit a single pattern. We call this one 'LayerResult'. Here are
-- some of its instances:
--
-- @type 'LayerResult' 'IdentityT' = 'Identity'
--type 'LayerResult' 'MaybeT' = 'Maybe'
--type 'LayerResult' ('ExceptT' e) = 'Either' e
--type 'LayerResult' ('RWST' r w s) = (,) w@
--
-- How exactly is it decided what @'LayerResult' t@ should be for a particular
-- monad transformer? There are several things to consider. Let's say we have
-- a monad transformer @t@ that we want to make an instance of
-- 'MonadTransControl'. Let's also assume that its 'LayerState' is @()@ (like
-- 'ExceptT', 'IdentityT' and 'MaybeT'), because this is easier to explain
-- first. The goal should be for @'LayerResult' t a@ to expand to a type
-- which is isomorphic to the type inside the @m@ computation wrapped by our
-- monad transformer. To take 'MaybeT' as an example again, it wraps an @m@
-- computation the type inside of which is @'Maybe' a@. Therefore we want
-- @'LayerResult' 'MaybeT' a@ to expands to @'Maybe' a@, which it does,
-- because @'LayerResult' 'MaybeT' = 'Maybe'@ as shown above.
--
-- The next thing to consider is that 'LayerResult' is a type family which
-- takes a monad transformer (of kind @(* -> *) -> * -> *@) as its argument,
-- and returns a type constructor @* -> *@. Note that it is not a type family
-- which takes a monad transformer @(* -> *) -> * -> *@ and a value @*@ and
-- returns a value @*@! So, we might be tempted to say:
--
-- @type 'LayerResult' 'IdentityT' a = a@
--
-- But we can't, because @'LayerResult' t@ must be a type constructor
-- @* -> *@, not a type @*@. The reason we do this is because we want the
-- compiler to able to infer @a ~ b@ if it knows that @'LayerResult' t a ~
-- 'LayerResult' t b@. It couldn't do this if we allowed definitions like the
-- above because
-- HWT(GHC/Type_families#Injectivity.2C_type_inference.2C_and_ambiguity,type families are not injective).
-- This is why we set @'LayerResult' 'IdentityT'@ to 'Identity'.
-- (@'Identity' a@ is of course isomorphic to @a@.)
--
-- Now, what if our monad transformer has a 'LayerState' that isn't just @()@?
-- Let's consider 'RWST' again. The type inside the @m@ computation wrapped by
-- 'RWST' is @(a, s, w)@, but its 'LayerResult' is @(,) w@. Now, @(,) w a@
-- expands to @(w, a@), which is isomorphic to @(a, w)@, but what about the
-- @s@ in the middle?
--
-- The answer is that monad transformers which have a 'LayerState' value often
-- update all or part of it as one of the side-effects of their computations.
-- This is the case with 'RWST', where the @s@ value you see in the \"result\"
-- is actually an updated value of the @s@ which is part of the 'LayerState'
-- of 'RWST', not part of the result proper.
--
-- This actually captures everything we need to implement the 'suspend'
-- operation we described above:
--
-- @'suspend' :: 'Monad' m => t m a -> 'LayerState' t -> m ('LayerEffects' t a)@
--
-- @type 'LayerEffects' t a = ('LayerResult' t a, 'LayerState' t)@
--
-- (The purpose of the 'LayerEffects' type synonym is twofold: it makes the
-- type signatures of 'suspend' and other operations a little bit less scary,
-- and it also communicates that the combination of a 'LayerResult' and a
-- 'LayerState' together reify the side-effects of a monad transformer.)
--
-- There are two important operations in the 'MonadTransControl' that we have
-- only alluded to so far: 'capture' and 'resume'.
--
-- @'capture' :: ('MonadTransControl' t, 'Monad' m) => t m ('LayerState' t)
--'resume' :: ('MonadTransControl' t, 'Monad' m) => 'LayerEffects' t a -> t m a@
--
-- 'capture' captures the current @'LayerState' t@ for the monad @t m@. This
-- is where the 'LayerState' that 'suspend' takes as its argument comes from.
-- 'resume' is the inverse of 'suspend': it takes the suspended side-effects
-- of a monad transformer @t@ reified by a @'LayerEffects' t a@ value, and
-- returns a returns a reconstructed computation of type @t m a@ with those
-- side-effects.
--
-- Taken together, we can use these operations to define @f'@, a lifted
-- version of the @f@ operation described above.
--
-- @f' :: ('MonadTransControl' t, 'Monad' (t m), 'Monad' m) => t m a -> t m b
--f' t = 'capture' '>>=' 'lift' '.' f '.' 'suspend' t '>>=' 'resume'@
--
-- The full instance for 'RWST' is given below:
--
-- @instance 'Monoid' w => 'MonadTransControl' ('RWST' r w s) where
--    type 'LayerResult' ('RWST' r w s) = (,) w
--    type 'LayerState' ('RWST' r w s) = (r, s)
--    'suspend' ('RWST' m) (r, s) = 'liftM' (\\(a, s', w) -> ((w, a), (r, s'))) (m r s)
--    'resume' ((w, a), (_, s)) = 'RWST' '$' \\_ _ -> 'return' (a, s, w)
--    'capture' = 'RWST' '$' \\r s -> 'return' ((r, s), s, 'mempty')
--    'extract' _ (_, a) = 'Right' a@
class (MonadTrans t, Functor (LayerResult t)) => MonadTransControl t where
    -- | The G(layerstate,layer state) of @t@.
    type LayerState (t :: TransformerKind) :: Type
    -- | The G(layerresult,layer result) of @t@.
    type LayerResult (t :: TransformerKind) :: MonadKind

    -- | Given a G(computation,computation) @m@ of type @t m a@ and the
    -- current G(layerstate,layer state) of the @t@ G(monadlayer,layer),
    -- 'suspend' suspends the G(sideeffect,side-effects) of @m@ which come
    -- from this G(monadlayer,layer) by returning a new
    -- G(computation,computation) in the monad @m@ that returns the
    -- G(layereffect,reification) of these G(sideeffects,side-effects) in
    -- a @'LayerEffects' t a@ value. This gives a version of @m@ which
    -- can be passed to G(controloperation,control operations) in the monad
    -- @m@.
    --
    -- The suspended G(sideeffect,side-effects) of the @t@
    -- G(monadlayer,layer) can later be recovered by 'resume'. This is
    -- expressed in the following law:
    --
    -- [Preservation] @'capture' '>>=' 'lift' '.' 'suspend' t '>>=' 'resume' ≡ t@
    suspend :: Monad m => t m a -> LayerState t -> m (LayerEffects t a)

    -- | Reconstructs a G(computation,computation) @t m a@ with the same
    -- G(sideeffect,side-effects) in the @t@ G(monadlayer,layer) as those
    -- G(layereffect,reified) by the given @'LayerEffects' t a@ value.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'capture' '>>=' 'lift' '.' 'suspend' t '>>=' 'resume' ≡ t@
    resume :: Monad m => LayerEffects t a -> t m a

    -- | Captures the current G(layerstate,layer state) of the @t@
    -- G(monadlayer,layer) of the monad @t m@.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'capture' '>>=' 'lift' '.' 'suspend' t '>>=' 'resume' ≡ t@
    capture :: Monad m => t m (LayerState t)

    -- | 'extract' inspects a G(layerresult,layer result), given by
    -- suspending the G(sideeffect,side-effects) in the @t@
    -- G(monadlayer,layer) of a G(computation,computation) of type @t m a@,
    -- and tries to \"extract\" an @a@ value from it. If it can't, this
    -- implies that @t@ G(shortcircuit,short-circuited).
    --
    -- Instances should satisfy the following laws:
    --
    -- [Preserve-Unit]
    --     @extractResult ('return' a) ≡ 'return' ('Just' a)@
    -- [Implies-Zero]
    --     @(extractResult m
    --         ≡ 'liftM' ('const' 'Nothing') m) ⇒ (∀f. m '>>=' f ≡ m)@
    --
    -- The @extractResult@ operation in terms of which these laws are defined
    -- is given by:
    --
    -- @extractResult :: forall t m a. ('MonadTransControl' t, 'Monad' (t m), 'Monad' m)
    --    => t m a
    --    -> t m ('Maybe' a)
    --extractResult t = do
    --    state <- 'capture'
    --    'lift' '$' do
    --        (result, _) <- 'suspend' t state
    --        'return' '$' 'extract' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' t) result@
    extract :: Proxy t -> LayerResult t a -> Either (LayerResult t b) a


------------------------------------------------------------------------------
-- | The G(layereffect,layer effects) of the @t@ G(monadlayer,layer) of the
-- monad @t m@.
type LayerEffects t a = (LayerResult t a, LayerState t)

