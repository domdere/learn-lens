-------------------------------------------------------------------
-- |
-- Module       : Data.Functor.Contravariant
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- A type class for contravariant Functors
--
-- these are like Functors but the variance goes the other way,
--
-- i.e If you have a Contravariant functor,
--
-- instead of
--
-- @
-- fmap :: (Functor f) => (a -> b) -> f a -> f b
-- @
--
-- you have
--
-- @
-- contramap :: (Contravariant f) => (a -> b) -> f b -> f a
-- @
--
-- (note `f a` and `f b` are swapped around)
--
-- This is swiped from Edward Kmett's `contravariant` library,
-- but that package contains a wealth of functions and etc..
--
-- This module presents just the class to stick to the bits that
-- are more directly relevent to understanding the lens material
-- presented here. (Also the intention here is to keep the
-- dependencies to a minimum)
--
-- See <http://hackage.haskell.org/package/contravariant> for the
-- full contravariant library
--
-------------------------------------------------------------------
module Data.Functor.Contravariant (
    -- * The Type Class
        Contravariant(..)
    ) where

import LensPrelude

infixl 4 >$

-- |
-- Subject to the following laws:
--
-- @
-- contramap id = id
-- contramap f . contramap g = contramap (g . f)
-- @
--
class Contravariant f where
    contramap :: (a -> b) -> f b -> f a

    -- |
    -- Replace all locations in the output with the same value. The default definition is `contramap` `.` `const`,
    -- but can be overridden with a more efficient version
    --
    (>$) :: b -> f b -> f a
    (>$) = contramap . const
