-------------------------------------------------------------------
-- |
-- Module       : Control.Lens.Getter
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- This Module takes a look at how Lenses interact with the Const
-- Functor.
--
-- Or "How Lenses work as Getters"
--
-------------------------------------------------------------------
module Control.Lens.Getter (
    -- * The Getting Type
        Getting
    -- * Operators
    ,   (^.)
    -- * Getter Lenses and the Reader Monad
    ,   view
    -- * Getter Lenses and the State Monad
    ,   use
    ) where

import LensPrelude
import Control.Monad.Reader ( MonadReader, asks )
import Control.Monad.State ( MonadState, gets )
import Data.Functor.Const ( Const(..) )

-- |
-- This type is `Lens s a` with the functor specialised to
-- `(Const r)`
type Getting r s a = (a -> Const r a) -> s -> Const r s

infixl 8 ^.

(^.) :: s -> Getting a s a -> a
x ^. l = getConst $ l Const x

-- |
-- Like `asks` but it takes a (Getter) lens instead of the function
--
view :: (MonadReader r m) => Getting a r a -> m a
view l = asks (^. l)

-- |
-- Like `gets` but takes a (getter) lens instead of a function
--
use :: (MonadState s m) => Getting a s a -> m a
use l = gets (^. l)
