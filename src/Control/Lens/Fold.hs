-------------------------------------------------------------------
-- |
-- Module       : Control.Lens.Fold
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Folds as/with Lenses!
--
-- The material here is taken from the `Control.Lens.Fold` module
-- the `lens` package
--
-------------------------------------------------------------------
module Control.Lens.Fold (
    -- * Types
        Fold
    -- * Operators
    ,   (^?)
    -- * Functions
    ,   folded
    ,   foldMapOf
    ) where

import LensPrelude
import Control.Lens.Getter
import Data.Contravariant
import Data.Functor.Const
import Data.Profunctor

import Control.Applicative ( Applicative, (*>),  pure )
import Data.Maybe ( Maybe(..) )
import Data.Monoid ( Monoid(..), First(..) )
import Data.Foldable ( Foldable(..) )

infixl 8 ^?

-- |
-- From the lens package:
--
-- A `Fold` describes how to retrieve multiple values in a way that can be composed
-- in a lens like fashion.
--
-- compare it to the definition of a `Getter`:
--
-- @
-- type `Getter` s a = (`Contravariant` f, `Data.Functor.Functor.Functor` f) => (a -> f a) -> s -> f s
-- @
--
-- So a `Fold` is a specialised `Getter`.
--
-- @`Const` r@ is the archetypal example of a `Data.Functor.Functor` that is also `Contravariant`.
--
-- See the `Applicative` instance for @`Const` m@ when @m@ is a `Data.Monoid.Monoid`.
--
-- A `Getter` is like a `Fold` that ignores the `Applicative` instance of @`Const` r@, i.e
-- it ignores any `Data.Monoid.Monoid` qualities of @r@ that `Const` could otherwise have accumulated.
--
type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

-- |
-- We can derive a `Data.Monoid.Monoid` algebra from
-- a `Contravariant` `Applicative` Functor
--
data Folding f a = Folding { getFolding :: f a } deriving (Show, Eq)

instance (Contravariant f, Applicative f) => Monoid (Folding f a) where
    mempty = Folding $ contramap (const ()) (pure ())

    (Folding x) `mappend` (Folding y) = Folding $ x *> y

(^?) :: s -> Getting (First a) s a -> Maybe a
x ^? l = getFirst $ foldMapOf l (First #. Just) x

-- type Getting r s a = (a -> Const r a) -> s -> Const r s
-- type Accessing p r s a = p a (Const r a) -> s -> Const r s

-- |
-- `^.` uses a specific @a -> `Const` a a@ relation in `Const`
--
-- `foldMapOf` is very similar, however it builds a relation
-- @a -> (`Const` r a)@ from a relation @a -> r@ by composing
-- it with `Const` and then does the same thing as `^.`.
--
-- It doesnt actually do any folding...
--
foldMapOf :: (Profunctor p) => Accessing p r s a -> p a r -> s -> r
foldMapOf l f x = getConst $ l (Const #. f) x

-- |
-- `folded` hasn't looked like this in a while, since Indexed Lenses have
-- been introduced, which I don't understand yet.
--
-- `Fold` looks close to a Traversal (see the type for `Data.Traversable.traverse`)
--
-- but see how `folded` uses `coerce` to cheat and coerce the final @f a@ into
-- @f (t a)@
--
folded :: (Foldable f) => Fold (f a) a
folded g = coerce . getFolding . foldMap (Folding . g)
