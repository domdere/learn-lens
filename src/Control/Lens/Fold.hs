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
    ,   (^..)
    -- * Functions
    ,   folded
    ,   foldMapOf
    ,   foldrOf
    ,   foldlOf
    ,   toListOf
    ) where

import LensPrelude
import Control.Lens.Getter
import Data.Contravariant
import Data.Functor.Const
import Data.Profunctor

import Control.Applicative ( Applicative, (*>),  pure )
import Data.Maybe ( Maybe(..) )
import Data.Monoid ( Monoid(..), Dual(..), Endo(..), First(..) )
import Data.Foldable ( Foldable(..) )

infixl 8 ^.., ^?

-- |
-- From the lens package:
--
--      A `Fold` describes how to retrieve multiple values in a way that can be composed
--      in a lens like fashion.
--
-- ------------------------
--
-- compare it to the definition of a `Getter`:
--
-- @
-- type `Getter` s a = (`Contravariant` f, `Data.Functor.Functor.Functor` f) => (a -> f a) -> s -> f s
-- @
--
-- The starting point for expressing Folds in a lens like way starts with `Data.Foldable.foldMap`
--
-- @
-- `Data.Foldable.foldMap` :: (`Data.Monoid.Monoid` m, `Data.Foldable.Foldable` t) => (a -> m) -> t a -> m
-- @
--
-- Compare this to `Data.Traversable.traverse`:
--
-- @
-- `Data.Traversable.traverse` :: (`Control.Applicative.Applicative` f, `Data.Traversable.Traversable` t) => (a -> f b) -> t a -> f (t b)
-- @
--
-- Which already looks like a lens, with @s ~ t a@, @t ~ t b@, @a ~ a@ and @b ~ b@ (and the additional requirement that @f@ is `Control.Applicative.Applicative`)
--
-- `Data.Foldable.foldMap` is not quite there unfortunately and requires some pretty sneaky rejiggering, first replace @m@ with `Const m a`:
--
-- @
-- type Fold t a = (`Data.Monoid.Monoid` m, `Data.Foldable.Foldable` t) => (a -> `Const` m a) -> t a -> `Const` m a
-- @
--
-- The problem is the final @`Const` m a@ that appears at the end as the final output.  If it were a @`Const` m (t a)@ instead,
-- it would fit the `Getter` pattern also.  Changing that type isn't so hard, see `coerce`.
--
-- So by using `coerce` at the end, we have:
--
-- @
-- type Fold t a = (`Data.Monoid.Monoid` m, `Data.Foldable.Foldable` t) => (a -> `Const` m a) -> t a -> `Const` m (t a)
-- @
--
-- Now the fact that @t@ is `Data.Foldable.Foldable` is not important.  The key feature of a function of the above type is that
-- it is structurally aware of @t a@ such that can find a (possibly empty) sequence of @a@s that it can map the @a -> `Const` m a@ function over
-- to get an ordered sequence of @m@s to `Data.Monoid.mappend` up.  A `Data.Foldable.Foldable` instance is only one such way to get a function
-- like this (see `folded`). For example, a `Getter` can provide a sequence of length 1.  The important thing to keep is the `Data.Monoid.Monoid` constraint.
--
-- So we'll generalise it a bit more:
--
-- @
-- type Fold t a = (`Data.Monoid.Monoid` m) => (a -> `Const` m a) -> s -> `Const` m s
-- @
--
-- We have something even more similar to a `Getter`.  Now lets review which qualities of the @`Const` m@ Functor we used...
--
-- Firstly, we used `coerce`:
--
-- @
-- coerce :: (`Contravariant` f, `Data.Functor` f) => f a -> f b
-- @
--
-- So if we were to replace @`Const` m@ with @f@ we would have to add the @`Contravariant` f@ constraint.
--
-- But what to do about the `Data.Monoid.Monoid` constraint?  See the `Control.Applicative` instance for `Const`.
-- This would be the mechanism that would be used internally to `Data.Monoid.mappend` up the @m@s, so we can
-- replace the @`Data.Monoid.Monoid m@ constraint with @`Control.Applicative f`@ instead:
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

-- |
-- This can be thought of as a "safe head" operator.
--
-- Remember that `Getting` is defined as:
--
-- @
-- type `Getting` r s a = (a -> Const r a) -> s -> Const r s
-- @
--
-- When @r@ is a `Data.Monoid.Monoid`, @`Getting` r s a@ is a `Fold`.
--
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
-- Remember:
--
-- @
-- type `Accessing` p r s a = p a (Const r a) -> s -> Const r s
-- @
--
foldMapOf :: (Profunctor p) => Accessing p r s a -> p a r -> s -> r
foldMapOf l f x = getConst $ l (Const #. f) x

-- |
--
-- The type is similar to this:
--
-- @
-- `foldrOf` :: Accessing p (`Data.Monoid.Endo` r) s a -> (a -> r -> r) -> r -> s -> r
-- @
--
-- `foldrOf` uses the @(a -> r -> r)@ function to map all the @a@s that the
-- @`Accessing p (`Data.Monoid.Endo` r) s a`@ lens/getter/fold/traversal gives access to,
-- and maps them into @r@ endomorphisms (@type `Data.Monoid.Endo` r = Endo { appEndo :: r -> r}@)
-- and then folds them using `foldMapOf`.  The `Data.Monoid.Monoid` behaviour
-- for `Data.Monoid.Endo` is like the `foldr` behaviour.
--
foldrOf :: (Profunctor p) => Accessing p (Endo r) s a -> p a (r -> r) -> r -> s -> r
foldrOf l f b = flip appEndo b #. foldMapOf l (Endo #. f)

-- |
-- A left associated fold of the parts of a structure exposed by a lens/fold/getter/traversal
--
-- The `Data.Monoid.Dual` type commutes the `Data.Monoid.mappend` operation,
-- which will reverse the order in which the endomorphisms are applied.
--
foldlOf :: Getting (Dual (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf l f b = flip appEndo b . getDual . foldMapOf l (Dual . Endo . flip f)

-- |
-- `folded` hasn't looked like this in a while, since Indexed Lenses have
-- been introduced, which I don't understand yet.
--
folded :: (Foldable f) => Fold (f a) a
folded g = coerce . getFolding . foldMap (Folding . g)

-- |
-- Given a lens/getter/fold/traversal, returns the list of
-- values it gives a view of.
--
toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []

-- |
-- Operator version of `toListOf`
--
(^..) :: s -> Getting (Endo [a]) s a -> [a]
(^..) = flip toListOf
