<!-- 
This is a literate haskell file and therefore requires a module declaration and
imports at the top. These are hidden so as not to be distracting to readers of
the article.

```haskell
{-# LANGUAGE Rank2Types, ConstraintKinds #-}
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, QuantifiedConstraints, UndecidableInstances #-} 
{-# LANGUAGE PolyKinds #-}
module Final where
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
class (d => c) => c ⊆ d
instance (d => c) => c ⊆ d

type f ≤ g = forall x. f x ⊆ g x

{-# LANGUAGE PolyKinds #-}
-- …
class (f &&& g) a
instance (f a, g a) => (f &&& g) a

-- monoapplicative - monopure
nullary :: (forall x. t x => x) -> Final t a
nullary x = Final $ \_ -> x

-- monofunctor - monomap
unary :: (forall x. t x => x -> x) -> Final t a -> Final t a
unary f ma = Final $ f . runFinal ma

-- monoapplicative - monoliftA2
binary :: (forall x. t x => x -> x -> x) -> Final t a -> Final t a -> Final t a
binary op ma mb = Final $ \f -> foldFinal f ma `op` foldFinal f mb

instance Semigroup ≤ t => Semigroup (Final t a) where
  (<>) = binary (<>)

instance Monoid ≤ t => Monoid (Final t a) where
  mempty = nullary mempty

instance Num ≤ t => Num (Final t a) where
  (+) = binary (+)
  (*) = binary (*)
  (-) = binary (-)
  abs = unary abs
  signum = unary signum
  fromInteger i = nullary (fromInteger i)

instance Fractional ≤ t => Fractional (Final t a) where
  (/) = binary (/)
  recip = unary recip
  fromRational r = nullary (fromRational r)

instance Floating ≤ t => Floating (Final t a) where
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
