-------------------------------------------------------------------
-- |
-- Module       : Control.Lens.Getter
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- This Module takes a look at how Lenses interact with the Const
-- Functor.
--
-- Or "How Lenses work as Getters"
--
-------------------------------------------------------------------
module Control.Lens.Getter (
    -- * The Getter Type
        Getter
    -- * Operators
    ,   (^.)
    ,   (.?)
    -- * Functions
    -- ** Building Getters
    ,   to
    -- ** Getter Lenses and the Reader Monad
    ,   view
    -- ** Getter Lenses and the State Monad
    ,   use
    ) where

import LensPrelude
import Data.Functor.Const
import Data.Contravariant

import Control.Monad.Reader ( MonadReader, asks )
import Control.Monad.State ( MonadState, gets )

-- |
-- This type is `Lens s a` with the functor specialised to
-- something that is both a Functor and in Contravariant
-- (the prototypical example being the Const functor)
--
-- @
--     type Getter' s a = (a -> Const a a) -> s -> Const a s
-- @
--
-- But this type lets you potentially use it on something isomorphic to (Const ())
--
type Getter s a = (Contravariant f, Functor f) => (a -> f a) -> s -> f s

infixl 8 ^.
infixr 9 .?

(^.) :: s -> Getter s a -> a
x ^. l = getConst $ l Const x

-- |
-- This works like `.` but composes getters in a similar way to the way the below
-- function would compose functions:
--
-- @
--     foo :: (Functor f) => (a -> f b) -> (b -> c) -> a -> f c
--     foo g h x = fmap h (g x)
-- @
--
(.?) :: (Functor f) => Getter s (f a) -> Getter a b -> Getter s (f b)
l1 .? l2 = to $ \x -> (^. l2) <$> (x ^. l1)

-- |
-- Like `asks` but it takes a (Getter) lens instead of the function
--
view :: (MonadReader r m) => Getter r a -> m a
view l = asks (^. l)

-- |
-- Like `gets` but takes a (getter) lens instead of a function
--
use :: (MonadState s m) => Getter s a -> m a
use l = gets (^. l)

-- |
-- With Getters, due to the `infer` function for `Const`
-- we don't need the "setter" that appears in `Control.Lens.Core.lens`
--
to :: (s -> a) -> Getter s a
to f g = contramap f . g . f
