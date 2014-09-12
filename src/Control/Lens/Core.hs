-------------------------------------------------------------------
-- |
-- Module       : Control.Lens.Core
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The Core Definitions for Lens
--
-------------------------------------------------------------------
module Control.Lens.Core (
    -- * The Final Lens Type
        Lens
    -- * A Lens like form with a specific Functor provided
    ,   LensLike
    -- * Simplified Lens Type
    ,   Lens'
    -- * Functions
    ,   lens
    ,   ftransposeOf
    ) where

import LensPrelude

type Lens s t a b = forall (f :: * -> *). (Functor f) => (a -> f b) -> s -> f t

type LensLike f s t a b = (a -> f b) -> s -> f t

-- | This is a simplified version of a lens, if you substitute s for t and a for b
-- in the definition for `Lens`, you will get a function that maps between f-algebras
--
type Lens' s a = Lens s s a a

-- | Creating a Lens is from a getter and a "setter" is pretty easy,
--
-- * `getter :: s -> a`
-- * `setter :: s -> b -> t`
--
-- If you start with the type, you'll probably be hard pressed to come up with something else
-- other than this implementation
--
lens :: (Functor f) => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
lens getIt packIt doSomething x = fmap (packIt x) ((doSomething . getIt) x)

-- |
-- If you have a lens relating @s (f a)@ to @f a@ for @f@ a Functor, `ftransposeOf` can map
-- it to f (s a)
--
ftransposeOf :: (Functor f) => Lens s t (f a) a -> s -> f t
ftransposeOf l = l id
