<!-- 
This is a literate haskell file and therefore requires a module declaration and
imports at the top. These are hidden so as not to be distracting to readers of
the article.

```haskell
{-# LANGUAGE Rank2Types, ConstraintKinds #-}
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, QuantifiedConstraints, UndecidableInstances #-} 
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Final where

import Data.Functor.Const
import Data.Functor.Identity
import Control.Monad (liftM, ap)
import Control.Monad.Cont
import Pair
```
import Data.Coerce
{-# LANGUAGE UndecidableSuperClasses #-}
,  ConstraintKinds

-->

```haskell
{-# LANGUAGE Rank2Types, ConstraintKinds #-}
-- …
newtype Final t a = Final { runFinal :: forall b. t b => (a -> b) -> b }
infix 0 `runFinal`

foldFinal :: t b => (a -> b) -> Final t a -> b
foldFinal = flip runFinal
```

`Final t a` is monadic in `a`:

```haskell
instance Functor (Final t) where
  fmap = liftM

instance Applicative (Final t) where
  pure a = Final $ \f -> f a
  (<*>) = ap

instance Monad (Final t) where
  ma >>= f = Final $ \g -> ma `runFinal` \a -> f a `runFinal` g
```

But it's also a monomorphically monadic in `t b => b`:


```haskell
-- mono-functor
omap :: (forall b. t b => b -> b) -> Final t a -> Final t a
omap f ma = Final $ f . runFinal ma

-- mono-applicative
opure :: (forall b. t b => b) -> Final t a
opure b = Final $ \_ -> b

oap :: (forall b. t b => b -> b -> b) -> Final t a -> Final t a -> Final t a
oap f m0 m1 = Final $ \g -> f (runFinal m0 g) (runFinal m1 g)

-- mono-monad
obind :: Final t a -> (forall b. t b => b -> Final t a) -> Final t a
obind ma f = Final $ \g -> f (runFinal ma g) `runFinal` g
```

Any f-algebra on `t b => b` can be lifted to an f-algebra on `Final t a`. 

```haskell
algebra :: Functor f => (forall b. t b => f b -> b) -> f (Final t a) -> Final t a
algebra f ma = Final $ \g -> f $ fmap (foldFinal g) ma where
```

This could be used to define equivalent mono-functor and mono-applicative instances:

```haskell
omap' :: (forall b. t b => b -> b) -> Final t a -> Final t a
omap' f = algebra (f . runIdentity) . Identity

opure' :: (forall b. t b => b) -> Final t a
opure' b = algebra (const b) (Const ())

oap' :: (forall b. t b => b -> b -> b) -> Final t a -> Final t a -> Final t a
oap' op m0 m1 = algebra (\(Pair a b) -> a `op` b) (Pair m0 m1)
```

`Final t a` creates a continuum between `Cont r a` (isomorphic to
`Final ((~) r) a`) and `Identity` (isomorphic to `Final Any a` via the Yoneda
lemma).

```haskell
fromCont :: Cont r a -> Final ((~) r) a
fromCont = Final . runCont

toCont :: Final ((~) r) a -> Cont r a
toCont = cont . runFinal

class Any a
instance Any a

fromIdentity :: Identity a -> Final Any a
fromIdentity (Identity a) = Final ($ a)

toIdentity :: Final Any a -> Identity a
toIdentity = Identity . foldFinal id
```

`Final t a` is also functorial in `t`:

```haskell
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, QuantifiedConstraints, UndecidableInstances #-}
-- …

-- Define a partial order on constraints.
--
-- Given `d => c`, the set of types with `d` instances is contained in the
-- set of types with `c` instances (e.g.  `Applicative => Functor`)
class (d => c) => d ⊆ c
instance (d => c) => d ⊆ c

-- Define a partial order on single-parameter typeclasses.
type f ≤ g = forall x. f x ⊆ g x

specify :: forall t t' a. t ≤ t' => Final t' a -> Final t a
specify ma = Final $ runFinal ma

{-# LANGUAGE PolyKinds #-}
-- …
class (f &&& g) a
instance (f a, g a) => (f &&& g) a
```

Using the standard reader trick for `->`, `Final t a` can often implement `t`:


```haskell
instance t ≤ Semigroup => Semigroup (Final t a) where
  (<>) = oap (<>)

instance t ≤ Monoid => Monoid (Final t a) where
  mempty = opure mempty

instance t ≤ Num => Num (Final t a) where
  (+) = oap (+)
  (*) = oap (*)
  (-) = oap (-)
  abs = omap abs
  signum = omap signum
  fromInteger i = opure (fromInteger i)

instance t ≤ Fractional => Fractional (Final t a) where
  (/) = oap (/)
  recip = omap recip
  fromRational r = opure (fromRational r)

instance t ≤ Floating => Floating (Final t a) where
  pi = opure pi

  exp = omap exp
  log = omap log

  cos = omap cos
  sin = omap sin
  tan = omap tan
  acos = omap acos
  asin = omap asin
  atan = omap atan

  cosh = omap cosh
  sinh = omap sinh
  tanh = omap tanh
  acosh = omap acosh
  asinh = omap asinh
  atanh = omap atanh
```

Notable exceptions like `Show` and `Eq` come from those typeclasses' member
functions that extract values from the instantiated type (e.g. `show :: Show a
=> a -> String`, `(==) :: Eq a => a -> a -> Bool`). 


```haskell
-- equivalence to the free monoid
toList :: Final Monoid a -> [a]
toList = foldFinal return

fromList :: [a] -> Final Monoid a
fromList as = Final $ \f -> foldMap f as

example :: t ≤ Floating => Final t ()
example = (pure () + 1) ** 2
```

```haskell
newtype Final1 t a x = Final1 { runFinal1 :: forall b. t b => (forall y. a y -> b y) -> b x }

foldFinal1 :: t b => (forall y. a y -> b y) -> Final1 t a x -> b x
foldFinal1 g mx = runFinal1 mx g

instance t ≤ Functor => Functor (Final1 t a) where
  fmap f mx = Final1 $ \g -> fmap f $ runFinal1 mx g

instance t ≤ Applicative => Applicative (Final1 t a) where
  pure a = Final1 $ \_ -> pure a
  mf <*> mx = Final1 $ \g -> runFinal1 mf g <*> runFinal1 mx g

instance t ≤ Monad => Monad (Final1 t a) where
  mx >>= f = Final1 $ \g -> runFinal1 mx g >>= \x -> runFinal1 (f x) g

lift :: a x -> Final1 t a x
lift ax = Final1 $ \g -> g ax

olift :: (forall b. t b => b x) -> Final1 t a x
olift bx = Final1 $ \_ -> bx

specify1 :: t ≤ t' => Final1 t' a x -> Final1 t a x
specify1 ma = Final1 $ runFinal1 ma

olift' :: forall t t' a x. t ≤ t' => (forall b. t' b => b x) -> Final1 t a x
-- olift' bx = Final1 $ \_ -> bx
olift' bx = specify1 (olift @t' bx)
```



## Literate Haskell

This README.md file is a literate haskell file, for use with
[`markdown-unlit`](https://github.com/sol/markdown-unlit#readme).  To allow GHC
to recognize it, it's softlinked as `Article.lhs`, which you can
compile with

    $ cabal build

Many of the above examples are
[`doctest`](https://github.com/sol/doctest#readme)-compatible, and can be run
with

    $ cabal test

Alternately, you can have cabal manage the dependencies and compile and test this with:
