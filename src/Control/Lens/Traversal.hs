-------------------------------------------------------------------
-- |
-- Module       : Control.Lens.Traversal
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- A look at traversals through the lens library
--
-------------------------------------------------------------------
module Control.Lens.Traversal (
    -- * Types
        Traversal
    -- * Functions
    ,   transposeOf
    -- * Common Traversals
    ,   both
    ) where

import LensPrelude
import Control.Lens.Core

import Control.Applicative ( Applicative(..), ZipList(..) )

-- |
-- The type of `Data.Traversable.traverse` is already pretty lens like:
--
-- @
-- `Data.Traversable.traverse` :: (`Data.Traversable.Traversable` t, `Control.Applicative.Applicative` f) => (a -> f b) -> t a -> f (t b)
-- @
--
-- Instead of all the manipulation we had to do with `Control.Lens.Fold.folded` and `Control.Lens.Setter.mapped` all we
-- have to do here is monomorphise the container to arrive at
--
-- @
-- type Traversal s t a b = forall f . (`Control.Applicative.Applicative` f) => (a -> f b) -> s -> f t
-- @
--
-- and we dont need to define something here like `Control.Lens.Fold.folded` or `Control.Lens.Setter.mapped`
-- because `Data.Traversable.traverse` is already exactly what we need.
--
type Traversal s t a b = forall f . Applicative f => (a -> f b) -> s -> f t

transposeOf :: LensLike ZipList s t [a] a -> s -> [t]
transposeOf l = getZipList . l ZipList

both :: Traversal (a, a) (b, b) a b
both f (x, y) = (,) <$> f x <*> f y
