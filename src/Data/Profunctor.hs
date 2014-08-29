-------------------------------------------------------------------
-- |
-- Module       : Data.Profunctor
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- A type class for Profunctors, Type constructors that take 2
-- Types, and are `Contravariant` in the first and `Covariant`
-- in the second.
--
-- This is swiped from Edward Kmett's `profunctors` library,
-- but that package contains a wealth of functions and etc..
--
-- This module presents just the class
-- and the relevant instances to stick to the bits that are more
-- directly relevent to understanding the lens material presented here.
-- (Also the intention here is to keep the dependencies to a minimum)
-- The Unsafe bits are also omitted.
--
-- See <http://hackage.haskell.org/package/profunctors> for the
-- full profunctors library if you wish to learn more.
--
--
-------------------------------------------------------------------
module Data.Profunctor (
    -- * The Type Class
        Profunctor(..)
    ) where

import LensPrelude

-- |
-- Minimal Complete Definition: `dimap` or both `lmap` and `rmap`
--
-- if you provide `dimap`, ensure:
--
-- @
-- `dimap` `id` `id` = `id`
-- @
--
-- if you provide `lmap` and `rmap`, ensure:
--
-- @
-- `lmap` `id` = `id`
-- `rmap` `id` = `id`
-- @
--
-- if you provide all three, make sure they are consistent:
--
-- @
-- `dimap` f g = `lmap` f `.` `rmap` g
-- @
--
-- Then from Parametricity these will follow:
--
-- @
-- `dimap` (f `.` g) (h `.` i) = `dimap` g h `.` `dimap` f i
-- `lmap` (f `.` g) = `lmap` g `.` `lmap` f
-- `rmap` (f `.` g) = `rmap` f `.` `rmap` g
-- @
--
class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g

    lmap :: (a -> b) -> p b c -> p a c
    lmap = flip dimap id

    rmap :: (b -> c) -> p a b -> p a c
    rmap = dimap id

instance Profunctor (->) where
    dimap g h f = h . f . g
