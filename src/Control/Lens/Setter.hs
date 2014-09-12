-------------------------------------------------------------------
-- |
-- Module       : Control.Lens.Setter
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- This Module takes a look at how Lenses interact with the Identity
-- Functor.
--
-- Or "How Lenses work as Setters"
--
-------------------------------------------------------------------
module Control.Lens.Setter (
    -- * The Plain Setter Type
        ASetter
    -- * The Settable Type Class
    ,   Settable
    ,   Setting
    -- * The Setter Type
    ,   Setter
    -- * Operators
    ,   (.~)
    -- * Functions
    ,   set
    ,   mapped
    ,   over
    ) where

import LensPrelude
import Data.Distributive
import Data.Profunctor
import Data.Functor.Identity

import Control.Applicative ( Applicative, pure )
import Data.Traversable ( Traversable )

infixr 4 .~

-- |
-- This is the specialisation of `Lens` on which the
-- Setters are based, it should look reminiscent of
-- the `CrapLens` in `Control..Lens.Motivation`
--
type ASetter s t a b = (a -> Identity b) -> s -> Identity t

-- |
--
type Setting p s t a b = p a (Identity b) -> s -> Identity t

-- |
-- This type class will embody the properties of the Identity Functor
-- That we actually take advantage of in the Setters, so we can experiment
-- in trying to apply them to other similar Functors.
--
class (Applicative f, Distributive f, Traversable f) => Settable f where
    untainted :: f a -> a

instance Settable Identity where
    untainted = runIdentity

-- |
-- This is the more general version that applies to all Functors that
-- share the properties of the `Identity` functor that we actually take advantage of.
-- This is the type that the lens library actually uses most of the time, so we'll use it
-- too so that its consistent with what you will see.  But in terms of understanding whats going
-- on, it will be useful to imagine `ASetter` in place of `Setter` when you see it in the types.
--
type Setter s t a b = forall f. (Settable f) => (a -> f b) -> s -> f t

-- |
-- sets the target of the setter to the given value
--
set :: ASetter s t a b -> b -> s -> t
set l x y = untainted $ l ((const . pure) x) y

(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set

-- |
-- applies a function to the target of a lens and sets the value to the output of the function
--
over :: (Profunctor p) => Setting p s t a b -> p a b -> s -> t
over l f x = untainted $ l (pure #. f) x

-- |
-- Generalises `Data.Functor.Functor` to a Setter
--
-- forall f. (Settable f1, Functor f) => (a -> f1 b) -> f a -> f1 (f b)
--
mapped :: (Functor f) => Setter (f a) (f b) a b
mapped g = pure . fmap (untainted . g)
