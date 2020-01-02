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
module Final where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Coerce
```
{-# LANGUAGE UndecidableSuperClasses #-}
,  ConstraintKinds

-->

```haskell
{-# LANGUAGE Rank2Types, ConstraintKinds #-}
-- …
newtype Final t a = Final { runFinal :: forall x. t x => (a -> x) -> x }

foldFinal :: t b => (a -> b) -> Final t a -> b
foldFinal = flip runFinal

```

`Final t a` is a continuum of types between `Cont r a` (isomorphic to `Final ((~) r) a`)
and `Identity` (isomorphic to `Final Any a` via the Yoneda lemma).

```haskell ignore
Final ((~) r) a
≈ forall x. (r ~ x) => (a -> x) -> x
≈ (a -> r) -> r
≈ Cont r a

class Any a
instance Any a

newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

Final Any a
≈ forall x. Any x => (a -> x) -> x
≈ forall x. (a -> x) -> x
≈ forall x. (a -> x) -> Identity x
≈ Yoneda Identity a
≈ Identity a
```

```haskell
instance Functor (Final t) where
  fmap f ma = Final $ \g -> runFinal ma (g . f)

instance Applicative (Final t) where
  pure a = Final $ \f -> f a
  mf <*> ma = Final $ \g -> runFinal mf $ \f -> runFinal ma $ g . f

toList :: Final Monoid a -> [a]
toList = foldFinal return

fromList :: [a] -> Final Monoid a
fromList as = Final $ \f -> foldMap f as

{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, QuantifiedConstraints, UndecidableInstances #-}
-- …
class (d => c) => d ⊆ c
instance (d => c) => d ⊆ c

type f ≤ g = forall x. f x ⊆ g x

specialize :: t' ≤ t => Final t a -> Final t' a
-- specialize ma = Final $ runFinal ma
specialize = coerce

{-# LANGUAGE PolyKinds #-}
-- …
class (f &&& g) a
instance (f a, g a) => (f &&& g) a

algebra :: Functor f => (forall x. t x => f x -> x) -> f (Final t a) -> Final t a
algebra f ma = Final $ \g -> f $ fmap (foldFinal g) ma

newtype Any t = Any { getAny :: forall a. t a => a }

any :: (forall x. t x => x) -> Any t
any x = Any x

-- monoapplicative - monopure
nullary :: forall t a. (forall x. t x => x) -> Final t a
-- nullary x = Final $ \_ -> x
-- nullary x = algebra (getAny . getConst) (Const (Any @t x))
nullary x = algebra (const x) (Const ())


-- monofunctor - monomap
unary :: (forall x. t x => x -> x) -> Final t a -> Final t a
-- unary f ma = Final $ f . runFinal ma
unary f = algebra (f . runIdentity) . Identity

data Pair a = Pair a a deriving Functor

-- monoapplicative - monoliftA2
binary :: (forall x. t x => x -> x -> x) -> Final t a -> Final t a -> Final t a
-- binary op ma mb = Final $ \f -> foldFinal f ma `op` foldFinal f mb
binary op ma mb = algebra (\(Pair a b) -> a `op` b) (Pair ma mb)


instance t ≤ Semigroup => Semigroup (Final t a) where
  (<>) = binary (<>)

instance t ≤ Monoid => Monoid (Final t a) where
  mempty = nullary mempty

instance t ≤ Num => Num (Final t a) where
  (+) = binary (+)
  (*) = binary (*)
  (-) = binary (-)
  abs = unary abs
  signum = unary signum
  fromInteger i = nullary (fromInteger i)

instance t ≤ Fractional => Fractional (Final t a) where
  (/) = binary (/)
  recip = unary recip
  fromRational r = nullary (fromRational r)

instance t ≤ Floating => Floating (Final t a) where
  pi = nullary pi

  exp = unary exp
  log = unary log

  cos = unary cos
  sin = unary sin
  tan = unary tan
  acos = unary acos
  asin = unary asin
  atan = unary atan

  cosh = unary cosh
  sinh = unary sinh
  tanh = unary tanh
  acosh = unary acosh
  asinh = unary asinh
  atanh = unary atanh
```

```haskell
-- newtype Final1 

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
