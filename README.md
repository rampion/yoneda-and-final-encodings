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
{-# LANGUAGE LambdaCase, EmptyCase #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
module Final where

import Data.Functor.Const
import Data.Functor.Identity
import Control.Monad (liftM, ap)
import Control.Monad.Cont
import Control.Monad.State
import Pair
```
import Data.Coerce
{-# LANGUAGE UndecidableSuperClasses #-}
,  ConstraintKinds

-->

```haskell
{-# LANGUAGE Rank2Types, ConstraintKinds #-}
-- …
newtype Final t a = Final { runFinal :: forall o. t o => (a -> o) -> o }
infix 0 `runFinal`

foldFinal :: t o => (a -> o) -> Final t a -> o
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

But it's also a monomorphically monadic in `t o => o`:


```haskell
-- mono-functor
omap :: (forall o. t o => o -> o) -> Final t a -> Final t a
omap f ma = Final $ f . runFinal ma

-- mono-applicative
opure :: (forall o. t o => o) -> Final t a
opure b = Final $ \_ -> b

oap :: (forall o. t o => o -> o -> o) -> Final t a -> Final t a -> Final t a
oap f m0 m1 = Final $ \g -> f (runFinal m0 g) (runFinal m1 g)

-- mono-monad
obind :: Final t a -> (forall o. t o => o -> Final t a) -> Final t a
obind ma f = Final $ \g -> f (runFinal ma g) `runFinal` g
```

Any f-algebra on `t o => o` can be lifted to an f-algebra on `Final t a`. 

```haskell
algebra :: Functor f => (forall o. t o => f o -> o) -> f (Final t a) -> Final t a
algebra f ma = Final $ \g -> f $ fmap (foldFinal g) ma where
```

This could be used to define equivalent mono-functor and mono-applicative instances:

```haskell
omap' :: (forall o. t o => o -> o) -> Final t a -> Final t a
omap' f = algebra (f . runIdentity) . Identity

opure' :: (forall o. t o => o) -> Final t a
opure' b = algebra (const b) (Const ())

oap' :: (forall o. t o => o -> o -> o) -> Final t a -> Final t a -> Final t a
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
-- Given `d => c`, the Put of types with `d` instances is contained in the
-- Put of types with `c` instances (e.g.  `Applicative => Functor`)
class (d => c) => d ⊆ c
instance (d => c) => d ⊆ c

-- Define a partial order on single-parameter typeclasses.
type f ≤ g = forall x. f x ⊆ g x

specify :: forall t t' a. t ≤ t' => Final t' a -> Final t a
specify ma = Final $ runFinal ma

handle :: (forall o. t o => a -> o) -> Final t (Either a as) -> Final t as
handle f me = Final $ \g -> me `runFinal` \case
  Left a -> f a
  Right as -> g as

{-# LANGUAGE PolyKinds #-}
-- …
class (f a, g a) => (f &&& g) a
instance (f a, g a) => (f &&& g) a

infixr 3 &&&
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
type (a ~> b) = forall x. a x -> b x

newtype FinalT t a x = FinalT { runFinalT :: forall o. t o => (a ~> o) -> o x }

foldFinalT :: t o => (a ~> o) -> (FinalT t a ~> o)
foldFinalT g mx = runFinalT mx g

instance t ≤ Functor => Functor (FinalT t a) where
  fmap f mx = FinalT $ \g -> fmap f $ runFinalT mx g

instance t ≤ Applicative => Applicative (FinalT t a) where
  pure a = FinalT $ \_ -> pure a
  mf <*> mx = FinalT $ \g -> runFinalT mf g <*> runFinalT mx g

instance t ≤ Monad => Monad (FinalT t a) where
  mx >>= f = FinalT $ \g -> runFinalT mx g >>= \x -> runFinalT (f x) g

data Void

absurd :: Void -> a
absurd = \case

data VoidT x

absurdT :: VoidT x -> a
absurdT = \case


collapseFinal :: t o => Final t Void -> o
collapseFinal = foldFinal absurd

collapseFinalT :: t o => FinalT t VoidT x -> o x
collapseFinalT = foldFinalT absurdT

data (f ||| g) x where
  This :: f x -> (f ||| g) x
  That :: g x -> (f ||| g) x

infixr 2 |||

handleT :: (forall o. t o => a ~> o) -> (FinalT t (a ||| as) ~> FinalT t as)
handleT f mx = FinalT $ \g -> mx `runFinalT` \case
  This ax   -> f ax
  That asx  -> g asx

-- monadic in a
fmapT :: (a ~> b) -> (FinalT t a ~> FinalT t b)
fmapT f mx = FinalT $ \g -> mx `runFinalT` (g . f)

pureT :: a ~> FinalT t a
pureT ax = FinalT $ \g -> g ax

-- dependent product
data Product a b x where
  Product :: a y -> b x -> Product a b x

zipT :: FinalT t a x -> (forall y. FinalT t b y) -> FinalT t (Product a b) x
zipT ma mb =
  ma `bindT` \a ->
  mb `bindT` \b ->
  pureT $ Product a b

newtype (a *~> b) x = K { runK :: a ~> b }

apT :: FinalT t (a *~> b) x -> (forall y. FinalT t a y) -> FinalT t b x
apT mf ma =
  mf `bindT` \f ->
  ma `bindT` \a ->
  pureT $ runK f a

bindT :: FinalT t a x -> (a ~> FinalT t b) -> FinalT t b x
bindT mx f = joinMapT f mx

joinMapT :: (a ~> FinalT t b) -> (FinalT t a ~> FinalT t b)
joinMapT f mx = FinalT $ \g -> mx `runFinalT` \ax -> f ax `runFinalT` g

-- monomorphically monadic in (t o => o x)
opureT :: (forall o. t o => o x) -> FinalT t a x
opureT bx = FinalT $ \_ -> bx

ojoinMapT :: (forall o x. t o => o x -> FinalT t a x) -> FinalT t a x -> FinalT t a x
ojoinMapT f mx = FinalT $ \g -> f (runFinalT mx g) `runFinalT` g

-- functorial in t
specifyT :: forall t t' a x. t ≤ t' => FinalT t' a x -> FinalT t a x
specifyT ma = FinalT (runFinalT ma)

data StateEff s x where
  Get :: StateEff s s
  Put :: s -> StateEff s ()

runStateEff :: (t ≤ MonadState s, t ≤ t')  => FinalT t' (StateEff s ||| as) x -> FinalT t as x
runStateEff = handleT (\case { Get -> get; Put s -> put s }) . specifyT

runStateEff' :: forall t s as x. FinalT t (StateEff s ||| as) x -> FinalT (MonadState s &&& t) as x
runStateEff' = handleT (\case { Get -> get; Put s -> put s }) . specifyT

this :: FinalT t a x -> FinalT t (a ||| as) x
this = fmapT This

that :: FinalT t as x -> FinalT t (a ||| as) x
that = fmapT That

example2 :: MonadState Integer m => m ()
example2 = collapseFinalT . runStateEff @(MonadState Integer) . this @Monad $ do
  i <- pureT Get
  pureT (Put (i + 1))

example2' :: MonadState Integer m => m ()
example2' = collapseFinalT . runStateEff' @Monad . this $ do
  i <- pureT Get
  pureT (Put (i + 1))

class a ∈ as where
  insert :: a x -> as x

instance a ∈ (a ||| as) where
  insert = This

instance a ∈ as => a ∈ (b ||| as) where
  insert = That . insert

example3 :: MonadState Integer m => m ()
example3 = collapseFinalT . runStateEff @(MonadState Integer) @Integer @Monad $ do
  i <- liftT $ Get @Integer
  liftT . Put $ i + 1

example3' :: MonadState Integer m => m ()
example3' = collapseFinalT . runStateEff' @Monad $ do
  i <- liftT $ Get @Integer
  liftT . Put $ i + 1

example4 :: (MonadState Integer m, Monad m) => m ()
example4 = foldFinalT @(MonadState Integer) (\case { Get -> get; Put s -> put s}) $ do
  i <- pureT $ Get @Integer
  pureT . Put $ i + 1

example5 :: FinalT (MonadState Integer) VoidT ()
example5 = joinMapT (\case { Get -> opureT get; Put s -> opureT $ put s}) $ do
  i <- pureT $ Get @Integer
  pureT . Put $ i + 1

emptyT :: (forall o. t o => a ~> o) -> (FinalT t a ~> FinalT t VoidT)
emptyT f = joinMapT (\ax -> opureT $ f ax)

example6 :: FinalT (MonadState Integer) VoidT ()
example6 = emptyT (\case { Get -> get; Put s -> put s}) $ do
  i <- pureT $ Get @Integer
  pureT . Put $ i + 1


liftT :: a ∈ as => a x -> FinalT t as x
liftT = pureT . insert
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
