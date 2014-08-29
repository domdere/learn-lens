-------------------------------------------------------------------
-- |
-- Module       : Control.Lens.Prism
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Prisms are traversals that can be used to model functions
-- that are injective but not onto. So in simple cases
-- (`Prism'`) you can have functions
--
-- @
-- f :: a -> s
-- @
--
-- and
--
-- @
-- fPartialInverse :: s -> Either s a
-- @
--
-- such that
--
-- @
-- fPartialInverse . f = Right
-- @
--
-- and
--
-- @
-- either id f . fPartialInverse = id
-- @
--
-- This is only scratching the surface, but as of this writing,
-- its the best understanding I have.
--
-------------------------------------------------------------------
module Control.Lens.Prism (
    -- * Types
        Prism
    -- * Functions
    ,   prism
    ) where

import LensPrelude
import Data.Choice
import Data.Profunctor

import Control.Applicative ( Applicative, pure )
import Data.Either ( Either, either )

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

-- Functions

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

