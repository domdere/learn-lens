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
    ) where

import LensPrelude

import Data.Functor ( Functor(..) )

newtype Const r a = Const { getConst :: r } deriving (Eq, Show)

instance Functor (Const r) where
    fmap _ (Const x) = Const x
