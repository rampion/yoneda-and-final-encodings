<!-- 
This is a literate haskell file and therefore requires a module declaration and
imports at the top. These are hidden so as not to be distracting to readers of
the article.

```haskell
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module YonedaAndFinalEncodings where
```

-->

The [Yoneda lemma](https://bartoszmilewski.com/2013/05/15/understanding-yoneda/)
is commonly encoded in Haskell as the understanding that `f a` is isomorphic to
`forall r. (a -> r) -> f r` aka `Yoneda f a`:

```haskell
{-# LANGUAGE Rank2Types #-}
-- ...
newtype Yoneda f a = Yoneda { runYoneda :: forall r. (a -> r) -> f r }

foldYoneda :: (a -> r) -> Yoneda f a -> f r
foldYoneda = flip runYoneda
```

This isomorphism relies on `f` being a `Functor`:

```haskell
toYoneda :: Functor f => f a -> Yoneda f a
toYoneda fa = Yoneda $ \f -> fmap f fa

fromYoneda :: Yoneda f a -> f a
fromYoneda = foldYoneda id
```

Something I stumbled across recently is that `Yoneda f` can still be quite
useful even when `f` is not a `Functor`.  In fact, it can be used to implement
[final encodings](https://peddie.github.io/encodings/encodings-text.html).

## Definitely *Not* a `Functor`

The `Obj t a` type packs packs the proof that type `a` has an instance
of typeclass `t` along with a value of type `a`.

```haskell
{-# LANGUAGE GADTs, ConstraintKinds #-}
-- ...
data Obj t a where
  Obj :: t a => a -> Obj t a

getObj :: Obj t a -> a
getObj (Obj a) = a
```

`Obj t` is definitely not a `Functor` for most values of `t`.  Given just
function `a -> b` and a value of type `Obj t a`, there's no way to prove that
type `b` has an instance of typeclass `t`:

```haskell
-- $
-- >>> :{
-- instance Functor (Obj t) where
--   fmap f (Obj a) = Obj (f a)
-- :}
-- ...
--     • Could not deduce: t b arising from a use of ‘Obj’
--       from the context: t a
--         bound by a pattern with constructor:
--                    Obj :: forall (t :: * -> Constraint) a. t a => a -> Obj t a,
--                  in an equation for ‘fmap’
-- ...
```

Consider, though `Yoneda (Obj t) a`: 

```haskell ignore
Yoneda (Obj t) a
~ forall b. (a -> b) -> Obj t b
~ forall b. t b => (a -> b) -> b -- XXX: this isn't true.
```

In english, this means that `Yoneda (Obj t) a` values can be converted to
values that are instances of `t`, given a way to convert `a`s.

For example, this means that `Yoneda Monoid a` and `[a]` are isomorphic:

```haskell
toList :: Yoneda (Obj Monoid) a -> [a]
toList = getObj . foldYoneda return


{-
-- XXX: Oops, argument breaks down here.
fromList :: [a] -> Yoneda (Obj Monoid) a
fromList as = Yoneda $ \f -> _ (foldMap f as)
  {-
  src/YonedaAndFinalEncodings.lhs:96:30: error:
      • Found hole: _ :: r -> Obj Monoid r
        Where: ‘r’ is a rigid type variable bound by
                 a type expected by the context:
                   forall r. (a -> r) -> Obj Monoid r
                 at src/YonedaAndFinalEncodings.lhs:96:15-45
      • In the expression: _
        In the expression: _ (foldMap f as)
        In the second argument of ‘($)’, namely ‘\ f -> _ (foldMap f as)’
      • Relevant bindings include
          f :: a -> r (bound at src/YonedaAndFinalEncodings.lhs:96:25)
          as :: [a] (bound at src/YonedaAndFinalEncodings.lhs:96:10)
          fromList :: [a] -> Yoneda (Obj Monoid) a
            (bound at src/YonedaAndFinalEncodings.lhs:96:1)
     |
  96 | fromList as = Yoneda $ \f -> _ (foldMap f as)
     |                              ^

  src/YonedaAndFinalEncodings.lhs:96:33: error:
      • No instance for (Monoid r) arising from a use of ‘foldMap’
        Possible fix:
          add (Monoid r) to the context of
            a type expected by the context:
              forall r. (a -> r) -> Obj Monoid r
      • In the first argument of ‘_’, namely ‘(foldMap f as)’
        In the expression: _ (foldMap f as)
        In the second argument of ‘($)’, namely ‘\ f -> _ (foldMap f as)’
     |
  96 | fromList as = Yoneda $ \f -> _ (foldMap f as)
     |                                 ^^^^^^^^^^^^
  -}
-}
```

<div style="display:none">

This packaging lets functions use functions defined by the specified typeclass
without specifying that it's required in the type signature:


```
{-# LANGUAGE FlexibleInstances #-}
-- |
-- >>> :set -XTypeApplications
-- >>> print $ Obj @Show @Int 5
-- Obj 5
-- >>> print $ Obj @Show (Just ())
-- Obj (Just ())
instance Show (Obj Show a) where
  showsPrec p (Obj a) = showParen (p >= 10) $ showString "Obj " . showsPrec 11 a
```

</div>




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
