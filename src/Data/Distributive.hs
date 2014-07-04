-------------------------------------------------------------------
-- |
-- Module       : Data.Distributive
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- A type class for Distributive Functors
--
-- The categorical dual of `Traversable`
--
-- with `distribute` being the dual for `sequenceA`
--
-- This is swiped from Edward Kmett's `distributive` library,
-- but that package contains a wealth of instances and etc..
--
-- This module presents just the class (with the Monad convenience functions omitted)
-- and the relevant instances to stick to the bits that are more
-- directly relevent to understanding the lens material presented here.
-- (Also the intention here is to keep the dependencies to a minimum)
--
-- See <http://hackage.haskell.org/package/distributive> for the
-- full distributive library, with a better description of what it
-- means to be distributive.
--
--
-------------------------------------------------------------------
module Data.Distributive (
    -- * The Type Class
        Distributive(..)
    -- * other functions
    ,   cotraverse
    ) where

import LensPrelude

import Data.Functor.Identity ( Identity(..) )

-- |
-- Minimal Complete Definition: `distribute` or `collect`
--
class (Functor g) => Distributive g where
    distribute :: (Functor f) => f (g a) -> g (f a)
    distribute = collect id

    collect :: (Functor f) => (a -> g b) -> f a -> g (f b)
    collect f = distribute . fmap f

cotraverse :: (Functor f, Distributive g) => (f a -> b) -> f (g a) -> g b
cotraverse f = fmap f . distribute

instance Distributive Identity where
    distribute = Identity . fmap runIdentity

instance Distributive ((->) r) where
--  distribute :: (Functor f) => f (r -> a) -> r -> f a
    distribute fh x = fmap ($ x) fh
