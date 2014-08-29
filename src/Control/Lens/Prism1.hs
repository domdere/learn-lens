-------------------------------------------------------------------
-- |
-- Module       : Control.Lens.Prism1
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Something similar to Prisms
--
-------------------------------------------------------------------
module Control.Lens.Prism1 (
    -- * Types
        Prism
    ,   prism
    ) where

import LensPrelude
import Data.Choice
import Data.Profunctor

import Control.Applicative ( Applicative, pure )
import Data.Either ( Either, either )

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
