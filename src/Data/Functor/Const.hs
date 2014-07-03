-------------------------------------------------------------------
-- |
-- Module       : Data.Functor.Const
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The Constant Functor.
--
-- Mix these with Lenses and you get Getters.
--
-------------------------------------------------------------------
module Data.Functor.Const (
    -- * The Const Functor
        Const(..)
    -- * Functions
    ,   inferConst
    ) where

import LensPrelude
import Data.Contravariant

newtype Const r a = Const { getConst :: r } deriving (Eq, Show)

instance Functor (Const r) where
    fmap _ (Const x) = Const x

instance Contravariant (Const r) where
    contramap _ (Const x) = Const x

-- | With Const since the input type gets thrown away, we can infer that to whatever type we want
--
inferConst :: Const r a -> Const r s
inferConst (Const x) = Const x
