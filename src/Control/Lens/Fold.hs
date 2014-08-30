-------------------------------------------------------------------
-- |
-- Module       : Control.Lens.Fold
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Folds with Lenses
--
-- This is a generalisation of the `Data.Foldable.Foldable`
-- typeclass,
--
-- In particular it seems to be most inspired by the
-- `Data.Foldable.foldMap` function:
--
-- @
-- `Data.Foldable.foldMap` :: (`Data.Foldable.Foldable` t, `Data.Monoid.Monoid` m) => (a -> m) -> t a -> m
-- @
--
-- Which looks pretty Lens like. As always, the idea gets tweaked a
-- bit to work with monomorphic containers, so its first
-- reformulated as:
--
-- @
-- type Fold s a = forall m. `Data.Monoid.Monoid` m => (a -> m) -> s -> m
-- @
--
-- And then just like the `Control.Lens.Getter.Getter` lens decorate it
-- with the `Data.Functor.Const` functor to get:
--
-- @
-- type Fold s a = forall m. `Data.Monoid.Monoid` m => `Control.Lens.Getter.Getting` m s a
-- @
--
-- The material here is taken from the `Control.Lens.Fold` module
-- the `lens` package
--
-------------------------------------------------------------------
module Control.Lens.Fold (
    -- * Types
        Fold
    -- * Operators
    ,   (^?)
    -- * Functions
    ,   foldMapOf
    ) where

import LensPrelude
import Control.Lens.Getter
import Data.Functor.Const
import Data.Profunctor

import Data.Maybe ( Maybe(..) )
import Data.Monoid ( Monoid, First(..) )

infixl 8 ^?

-- |
-- A `Fold` allows you to extract multiple results from a container.
--
-- A `Getter` is a `Fold` that ignores the `Data.Monoid.Monoid` properties
--
type Fold s a = forall m. Monoid m => Getting m s a

(^?) :: s -> Getting (First a) s a -> Maybe a
x ^? l = getFirst $ foldMapOf l (First . Just) x

-- type Getting r s a = (a -> Const r a) -> s -> Const r s
-- type Accessing p r s a = p a (Const r a) -> s -> Const r s

-- |
-- `^.` uses a specific @a -> `Const` a a@ relation in `Const`
--
-- `foldMapOf` is very similar, however it builds a relation
-- @a -> (`Const` r a)@ from a relation @a -> r@ by composing
-- it with `Const` and then does the same thing as `^.`.
--
-- It doesnt actually do any folding...
--
foldMapOf :: (Profunctor p) => Accessing p r s a -> p a r -> s -> r
foldMapOf l f x = getConst $ l (rmap Const f) x
