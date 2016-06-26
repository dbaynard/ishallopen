{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes, TypeOperators, LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Lens (
    module Lens
)   where

import BasicPrelude
import Data.Profunctor
import Data.Functor.Identity

import Data.Coerce

import Control.Applicative
import Control.Monad.State.Strict

-- * Types

-- ** Base types

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Traversal s t a b = forall f . Applicative f => (a -> f b) -> s -> f t

type Prism s t a b = forall p f . (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Iso s t a b = forall p f . (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- ** Simplified types

type s :~> a = Lens s s a a
type s :~>> a = Traversal s s a a
type s :~>: a = Prism s s a a
type s :~: a = Iso s s a a

-- ** Types for implementing generic lens functions

type Getting r s a = (a -> Const r a) -> (s -> Const r s)

type Setting p s t a b = p a (Identity b) -> s -> Identity t

-- * Construct optics

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

-- * Apply optics

data Exchange a b s t = Exchange (s -> a) (b -> t)
                      deriving (Functor)

instance Profunctor (Exchange a b) where
    dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
    {-# INLINE dimap #-}
    lmap f (Exchange sa bt) = Exchange (sa . f) bt
    {-# INLINE lmap #-}
    rmap g (Exchange sa bt) = Exchange sa (g . bt)
    {-# INLINE rmap #-}

--type AnIso s t a b = (a -> a, b -> Identity b) -> (s -> a, b -> Identity t)
type AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)

withIso :: AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id Identity) of
    Exchange sa bt -> k sa (runIdentity . bt)
{-# INLINE withIso #-}

from :: AnIso s t a b -> Iso b a t s
from i = withIso i $ flip iso
{-# INLINE from #-}

{-|
  Also known as 'over', '%~' uses the supplied function to replace values within data types

  > l %~ f = runIdentity . l (Identity . f)

  For ':~>' the type simplifies to

  > (%~) :: s :~> a -> (a -> a) -> s -> s

  'over' has not been implemented.
-}
(%~) :: Setting (->) s t a b -> (a -> b) -> s -> t
l %~ f = runIdentity . l (Identity . f)
{-# INLINE (%~) #-}
infixr 4 %~

{-|
  Also known as 'modifying', '%=' uses the supplied function to replace the state

  > l %= f = modify $ l %~ f

  For ':~>' the type simplifies to

  > (%=) :: s :~> a -> (a -> a) -> s -> s

  'modifying' has not been implemented.
-}
(%=) :: MonadState s m => Setting (->) s s a b -> (a -> b) -> m ()
l %= f = modify $ l %~ f
{-# INLINE (%=) #-}
infixr 4 %=

{-|
  Also known as 'assign', '.=' assigns the state in a 'StateT' environment to that supplied

  > l .= v = modify $ l %~ const v
  nad-ST

  For ':~>' the type simplifies to

  > (.=) :: MonadState s m => s :~> a -> a -> m ()

  '.=' conflicts with 'Data.Aeson..=' from "Data.Aeson"

  'assign' has not been implemented.
-}
(.=) :: MonadState s m => Setting (->) s s a b -> b -> m ()
l .= v = modify $ l %~ const v
{-# INLINE (.=) #-}
infix 4 .=

(.$=) :: (MonadState s m, Functor f) => Setting (->) s s a b -> f b -> f (m ())
{-# INLINE (.$=) #-}
l .$= v = fmap (l .=) v
infixr 4 .$=

{-|
  Assign state to a lens

  Use as 
  > do
  >     lens <~ action
-}
(<~) :: MonadState s m => Setting (->) s s a b -> m b -> m ()
l <~ mb = mb >>= (l .=)
{-# INLINE (<~) #-}
infix 2 <~

{-|
  Also know (flipped) as 'view', '^.' extracts the value from a record 's' of type 'a' by
  the given 'Lens'.

  'view' is more useful in function composition.
-}
(^.) :: s -> Getting a s a -> a
s ^. l = getConst . l Const $ s
{-# INLINE (^.) #-}
infixl 8 ^.

view :: Getting a s a -> s -> a
{-# INLINE view #-}
view l s = s ^. l

{-|
  Also known as 'set', '.~' uses the supplied values to replace values within data types

  > l .~ v = runIdentity . l (\_ -> Identity b)

  > l .~ v = l %~ const v

  For ':~>' the type simplifies to

  > (%~) :: s :~> a -> a -> s -> s

  'set' has not been implemented.
-}
(.~) :: Setting (->) s t a b -> b -> s -> t
{-# INLINE (.~) #-}
l .~ v = l %~ const v
infixr 4 .~

(.$~) :: Functor f => Setting (->) s t a b -> f b -> f (s -> t)
{-# INLINE (.$~) #-}
l .$~ v = fmap (l .~) v
infixr 4 .$~

-- ** Prisms

{-
 -preview :: s :~>: a -> s -> Maybe a
 -preview = undefined
 -}

{-
 -review :: s :~>: a -> a -> s
 -review = coerce
 -}

-- * Samples

-- ** Lenses

_1 :: (one,two) :~> one
_1 f (one,two) = (\one -> (one,two)) <$> f one
{-# INLINE _1 #-}

_2 :: (one,two) :~> two
_2 f (one,two) = (\two -> (one,two)) <$> f two
{-# INLINE _2 #-}

-- ** Prisms

_Left :: Either left right :~>: left
{-
 -_Left = prism' Left $ \case
 -                        Left x -> pure x
 -                        _   -> empty
 -}
_Left = prism Left (either Right (Left . Right))
{-# INLINE _Left #-}

_Right :: Either left right :~>: right
{-
 -_Right = prism' Right $ \case
 -                        Right x -> pure x
 -                        _   -> empty
 -}
_Right = prism Right (either (Left . Left) Right)
{-# INLINE _Right #-}

