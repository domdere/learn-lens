-------------------------------------------------------------------
-- |
-- Module       : LensPrelude
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The Prelude for this project
--
-------------------------------------------------------------------
module LensPrelude (
    -- * TypeClasses
        Eq(..)
    ,   Functor(..)
    ,   Integral(..)
    ,   Num(..)
    ,   Show(..)
    -- * Types
    ,   Bool(..)
    -- * Operators
    ,   ($)
    ,   (.)
    ,   (<$>)
    -- * Functions
    ,   const
    ,   flip
    ,   id
    ,   on
    ) where

import Prelude ( Eq(..), Integral(..), Num(..), Show(..), ($) )
import Data.Bool ( Bool(..) )
import Data.Function ( (.), const, flip, id, on )
import Data.Functor ( Functor(..), (<$>) )


