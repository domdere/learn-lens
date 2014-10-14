-------------------------------------------------------------------
-- |
-- Module       : Data.Bifoldable
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- A type of kind (* -> * -> *) that is foldable over its two
-- parameters.
--
-------------------------------------------------------------------
module Data.Bifoldable (
        Bifoldable(..)
    ) where

import LensPrelude

import Data.Monoid( Monoid(..), Dual(..), Endo(..) )

-- |
-- This class is for type constructors of kind (* -> * -> *).
-- if @p@ is in this class then every value of @p a b@
-- has an inherent sequence of intermixed a's and b's, which can
-- be computed like so:
--
-- @
-- bifoldr (\x es -> (Left x) : es) (\y es -> (Right x) : es) [] :: p a b -> [Either a b]
-- @
--
-- Which can result in a list of intermixed Left and Right Either values...
-- (So its not all Lefts and then all Rights, etc...)
--
class Bifoldable p where
    -- | Folds over both types
    --
    -- @
    -- 'bifold' == 'bifoldMap' id id
    -- @
    --
    bifold :: (Monoid m) => p m m -> m
    bifold = bifoldMap id id
    {-# INLINE bifold #-}

    -- |
    -- @
    -- 'bifoldMap' f g == 'bifoldr' ('mappend' . f) ('mappend' . g) 'mempty'
    -- @
    --
    bifoldMap :: (Monoid m) => (a -> m) -> (b -> m) -> p a b -> m
    bifoldMap f g = bifoldr (mappend . f) (mappend . g) mempty
    {-# INLINE bifoldMap #-}

    bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
    bifoldr f g z t = appEndo (bifoldMap (Endo . f) (Endo . g) t) z
    {-# INLINE bifoldr #-}

    bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
    bifoldl f g z t = appEndo (getDual (bifoldMap (Dual . Endo . flip f) (Dual . Endo . flip g) t)) z
    {-# INLINE bifoldl #-}
