Motivation For Lens
===================

Setup
-----

> module Control.Lens.Motivation where

> import LensPrelude
> import Data.Functor.Const

> import Data.Tuple ( fst, snd )

A Common Scenario
-----------------

You have a value of a type `s`, and from that you wish to get a value of type `a`, do something to it, and then pack it back into the input


In summary, you could represent this common computation:

-   A input `x` of type `s`
-   A function of type `s -> a` that gets the bit you want to work with out (which I'll refer to as the *getter*).
-   A function of type `a -> a` that does something to that bit (which I'll refer to as the *work*).
-   A function of type `s -> a -> s` that gets the result of the work and the origin input and "packs" the result of the *work* back in the original input (which i'll refer to as the *setter*)

You can imagine working with the `State` Monad would involve a lot these kinds of operations.

A First Attempt at a Lens
-------------------------

So if we were to write up a general function that embodies this control flow, we might write a function with this type:

< protolens' :: s -> (s -> a) -> (a -> a) -> (s -> a -> s) -> s

But (for reasons we'll elaborate on later) we'll re arrange the arguments and write it like so:

> protolens' :: (s -> a) -> (s -> a -> s) -> (a -> a) -> s -> s
> protolens' getter setter work x = setter x (work (getter x))

The first two arguments are a getter and a setter. These are technically inherent to the relationship between the value of type `s` and `a`. As in how do you get a `a` value from an `s` value, and how do you update the `s` value with a new `a` value to get a new `s` value
These concepts are what a lens value embodies, or at least what we want it to embody.

Lets look at a 2-tuple as an example.

Consider the following (`s ~ (b, c)` and `a ~ b`):

> _1' :: (b -> b) -> (b, c) -> (b, c)
> _1' = protolens' fst (\(_, y) x' -> (x', y))

`_1'` as a "Lens" value encapsulates how you would get the first value out of a 2-tuple, and put the result of the "generic" work you would do on it back in to the tuple.

We'll quickly define the same thing for the second field:

> _2' :: (c -> c) -> (b, c) -> (b, c)
> _2' = protolens' snd (\(x, _) y' -> (x, y'))

`_1'` are `_2'`, are basically of this form:

> type CrapLens s a = (a -> a) -> s -> s

It maps an endomorphism on `a` to an endomorphism on `s`.

This is more generally what lenses are trying to get at, the "operations" involving getters and setters that `protolens'` can produce only represent a subset
of computations that `CrapLens` can embody.

For e.g consider this function on 2-tuple morphisms for which no appropriate
"getter" exists for the generic case:

> both' :: (b -> b) -> (b, b) -> (b, b)
> both' f (x, y) = (f x, f y)

Lets play around with it a bit.

We can compose them:

```
ghci> import Control.Lens.Motivation
ghci> import Data.Functor ( fmap )

ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

ghci> let craplens1 = fmap; craplens1 :: CrapLens [a] a
ghci> :t (craplens1 . _1')
(craplens1 . _1') :: (b -> b) -> [(b, c)] -> [(b, c)]

ghci> :t (craplens1 . _1') (*100)
(craplens1 . _1') (*100) :: Num b => [(b, c)] -> [(b, c)]

ghci> (craplens1 . _1') (*100) [(1,3), (9,7)]
[(100,3),(900,7)]

ghci> (craplens1 . _2') (*100) [(1,3), (9,7)]
[(1,300),(9,700)]

ghci> (craplens1 . both') (*100) [(1,3), (9,7)]
[(100,300),(900,700)]

ghci> :t (_2' . craplens1)
(_2 . craplens1) :: (a -> a) -> (b, [a]) -> (b, [a])

ghci> (_2' . craplens1) (*2) ("hello", [1,2,3,4,5,6,7])
("hello",[2,4,6,8,10,12,14])
```

So you can see `craplens1` is a lens that takes a morphism on `a` and gives you a morphism on lists of `a` (just using `fmap` to send that morphism to all the elements), composing it with `_1` first sends
a morphism `b -> b` to a morphism `(b, c) -> (b, c)` and then to a morphism `[(b, c)] -> [(b, c)]`, and so on.

So far this isn't much of a leap beyond other techniques you probably already use. For example `(a, b)` is already an instance of the `Bifunctor` type class which has functions `first` and `second` to map functions to the first and second field respectively.
So `(craplens1 . _1') (*100) [(1,3), (9,7)]` can be rewritten as something like `(fmap . first) (*100) [(1,3), (9, 7)]`, and similarly `(craplens1' . _2) (*100) [(1,3), (9,7)]` as `(fmap . second) (*100) [(1,3), (9, 7)]`.

But it does encourage you to think a little more generically, `fmap`, `second` and `first` are all very similar concepts and are only one type of "morphisms on morphisms".

After all `fmap` only maps functions of the form `a -> b` to functions of the form `f a -> f b`, there are more cases to consider where the types are not related by type constructors.

But But But!
------------

You've probably noticed at this point that the definition of `CrapLens` is actually *less* powerful than `fmap` and its ilk, after all `fmap` can map functions of the form `a -> b` while our
lenses only work on Endomorphisms (functions of the form ( `a -> a` )). Likewise `f a` and `f b` are distinct types for `a` and `b` distinct.

Hence something like this doesn't work:

```
ghci> import Data.Char
ghci> :t const
const :: a -> b -> a

ghci> :t const 'h'
const 'h' :: b -> Char

ghci> :t (_1' . _2') (const 'h')
(_1' . _2') (const 'h') :: ((b, Char), c) -> ((b, Char), c)

ghci> (_1' . _2') (const 'h') ((1, 2), 3)

\<interactive>:37:30:
    Could not deduce (Num Char) arising from the literal ‘2’
    from the context (Num b, Num c)
      bound by the inferred type of
               it :: (Num b, Num c) => ((b, Char), c)
      at <interactive>:37:1-35
    In the expression: 2
    In the expression: (1, 2)
    In the second argument of ‘_1' . _2'’, namely ‘((1, 2), 3)’
```

As you can see from the type of `const 'h'` it should be able to map a value of *any* type to a value of type `Char` but the type of `(_1' . _2') (const 'h')` explicitly expects a value of type `Char` in the expected place.

Also on another note, we also aren't leveraging the type system as much as we could,
consider the following incorrect implementations for `protolens`:

> badprotolens :: (s -> a) -> (s -> a -> s) -> CrapLens s a
> badprotolens _ _ _ x = x

> badprotolens1 :: (s -> a) -> (s -> a -> s) -> CrapLens s a
> badprotolens1 getter setter _ x = setter x (getter x)

Both cases are incorrect as they skip steps in the outlined scenario at the beginning,
`badprotolens` skips everything and just returns the input, while `badprotolens1` does
the getting and the setting but skips the work.

They are both wrong but still typecheck, because we didn't express that scenario properly
in the type of `protolens'`

Lets quickly amend our definition of Lens to allow for it to be a mapping of more general morphisms, and the new type for `protolens` will make incorrect implementations impossible:

> type BetterLens s t a b = (a -> b) -> s -> t

And quickly fix up the functions and lenses we had defined before:

> protolens :: (s -> a) -> (s -> b -> t) -> (a -> b) -> s -> t
> protolens getter setter work x = setter x (work (getter x))

> _1 :: BetterLens (a, b) (a', b) a a'
> _1 = protolens fst (\(_, y) x' -> (x', y))

> _2 :: BetterLens (a, b) (a, b') b b'
> _2 = protolens snd (\(x, _) y' -> (x, y'))

> both :: BetterLens (a, a) (a', a') a a'
> both f (x, y) = (f x, f y)

And try again:

```
ghci> import Data.Char
ghci> :t (_1 . _2) (const 'h')
(_1 . _2) (const 'h') :: ((a, b1), b) -> ((a, Char), b)

ghci> (_1 . _2) (const 'h') ((1,2), 3)
((1,'h'),3)
```

And what we have now is *more* general than `fmap` and its ilk (`s` is more general than `f a` and `t` is more general than `f b`).

Limitations of our definition
-----------------------------

Technically, there are no limitations (that I can think of at least) for this definition of Lens that wouldn't exist in the one the lens library starts off with.
This is because the definition we have here is actually more general than the working definition of a Lens in the lens library.

For instance, while we have demonstrate how we can use lenses as "setters" it may not be immediately obvious how to get a "getter"
from a lens as we have described it. And indeed it may not be obvious if you looked at the definition in the lens library either, but it does provide some hints...

For the sake of exposition and to whet the appetite for things to come I'll generate a specialisation of `BetterLens` that can generate a getter...
(don't worry about understanding whats going on here at the moment, you only need to appreciate that its possible and it works)

> type FunctorLens s t a b = (Functor f) => BetterLens s (f t) a (f b)

> flens :: (s -> a) -> (s -> b -> t) -> FunctorLens s t a b
> flens getter setter work x = fmap (setter x) (work (getter x))

> flget :: FunctorLens s s a a -> s -> a
> flget l x = getConst (l Const x)

> _1'' :: FunctorLens (a, b) (a', b) a a'
> _1'' = flens fst (\(_, y) x' -> (x', y))

> _2'' :: FunctorLens (a, b) (a, b') b b'
> _2'' = flens snd (\(x, _) y' -> (x, y'))

And it works:

```
ghci> import Control.Lens.Motivation
ghci> import Data.Tuple

ghci> :t flget _1''
flget _1'' :: (a, b) -> a

ghci> :t fst
fst :: (a, b) -> a

ghci> :t flget _2''
flget _2'' :: (a1, a) -> a

ghci> :t snd
snd :: (a, b) -> b

ghci> flget _1'' (1, 2)
1

ghci> flget _2'' (1, 2)
2

ghci> flget _1'' ((1, 2), 3)
(1,2)

ghci> flget (_1'' . _2'') ((1, 2), 3)
2

ghci> flget (_1'' . _1'') ((1, 2), 3)
1
```

You may be asking whether or not the previous behaviour exhibited by `_1` and `_2` is maintained by `_1''` and `_2''`, or do you keep having to pull out different lenses for doing types of "work".

The short answer is no.  The most general lens will be able to do all kinds of "work".  We'll look at the long answer and how to get the previous behaviour out of this type in the rest of the material.

So, the working definition for a lens that the rest of the material will start from is actually a slightly *specialised* version of the definition we have here.

We have

< type BetterLens s t a b = (a -> b) -> s -> t

While the actual definition of `Lens` used in the library is this:

> type Lens s t a b = (Functor f) => (a -> f b) -> s -> f t

Note that this is *still* more general than `fmap` in that there are more possibilities for the relationship between `a` and `s` than there are for `a and `f a`, similarly for `b` and `f t`.

The intent here is to try and generalise the concept of "work" (Recall that I had said the `a -> b` argument which is now of type `a -> f b` corresponded the the "work" you wanted to do with
the `a` value before recombining it with `s` ).

The "work" is no longer something that takes a value of type `a` and returns something of type `b`, but rather something that has some sort of structure on top `b` that you use in combination with `fmap`
and the "setter" function to turn the `b`s within that structure into `t`s...

The majority of the material will be about what we can achieve with Lenses by specialising the `Functor f`, either with some specific functor, or by adding more constraints.
