-------------------------------------------------------------------
-- |
-- Module       : Data.Choice
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- This is ripped off from `Data.Profunctor` in the `profunctors`
-- package
--
-------------------------------------------------------------------
module Data.Choice (
    -- * Prismatic Profunctors
        Choice(..)
    ) where

import LensPrelude
import Data.Profunctor

import Data.Either ( Either(..), either )

class (Profunctor p) => Choice p where
    left' :: p a b -> p (Either a c) (Either b c)
    right' :: p a b -> p (Either c a) (Either c b)

instance Choice (->) where
--  left' :: (a -> b) -> Either a c -> Either b c
    left' f = either (Left . f) Right

--  right' :: (a -> b) -> Either c a -> Either c b
    right' f = either Left (Right . f)
