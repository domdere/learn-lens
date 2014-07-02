{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
-------------------------------------------------------------------
-- |
-- Module       : Control.Lens.Tuple
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Lenses for n-Tuples.
--
-------------------------------------------------------------------
module Control.Lens.Tuple (
    -- * Type Classes
        Field1(..)
    ,   Field2(..)
    ) where

import LensPrelude
import Control.Lens.Core

import Data.Functor.Identity ( Identity(..) )

-- |
-- This is a type class for types that have a first field,
-- I.e one-tuples and higher.
-- its fairly complicated looking
-- (And the version in Lens has all kinds of voodoo sprinkled over 
-- it to speed it up with its inlines etc..)
-- but its basically saying the input types, [s] and [t]
-- each individually determine [a] and [b] (the respective
-- output types)
-- [s] and [b] determine [t] while [t] and [a] determine [s]
--
-- This type class requires the FlexibleInstances extension
-- So it seems sort of disgusting to me, but its the least restrictive
-- form and was written by people smarter than I.
--
class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _1 :: Lens s t a b

instance Field1 (Identity a) (Identity b) a b where
    _1 f (Identity x) = Identity <$> f x

instance Field1 (a, b) (a', b) a a' where
    _1 f (x, y) = (\x' -> (x', y)) <$> f x

instance Field1 (a, b, c) (a', b, c) a a' where
    _1 f (x, y, z) = (\x' -> (x', y, z)) <$> f x

instance Field1 (a, b, c, d) (a', b, c, d) a a' where
    _1 f (x, y, z, z1) = (\x' -> (x', y, z, z1)) <$> f x

-- | Same deal but for the second field
--
class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _2 :: Lens s t a b

instance Field2 (a, b) (a, b') b b' where
    _2 f (x, y) = (\y' -> (x, y')) <$> f y

instance Field2 (a, b, c) (a, b', c) b b' where
    _2 f (x, y, z) = (\y' -> (x, y', z)) <$> f y

instance Field2 (a, b, c, d) (a, b', c, d) b b' where
    _2 f (x, y, z, z1) = (\y' -> (x, y', z, z1)) <$> f y


-- | Same deal but for the third field
--
class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _3 :: Lens s t a b

instance Field3 (a, b, c) (a, b, c') c c' where
    _3 f (x, y, z) = (\z' -> (x, y, z')) <$> f z

instance Field3 (a, b, c, d) (a, b, c', d) c c' where
    _3 f (x, y, z, z1) = (\z' -> (x, y, z', z1)) <$> f z

-- | Same deal but for the fourth field
--
class Field4 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _4 :: Lens s t a b

instance Field4 (a, b, c, d) (a, b, c, d') d d' where
    _4 f (x, y, z, z1) = (\z1' -> (x, y, z, z1')) <$> f z1

