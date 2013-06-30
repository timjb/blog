---
title: Flexibly-Phased Constraints in Haskell
---

Unfortunately, Haskell's Prelude abounds with partial functions, like
`head`, `tail`, `read`, etc. There have been some attempts to resolve
this by replacing them with safe variants that return `Maybe a` rather
than `a`. But what about when we can make static guarantees about data?

Because Haskell doesn't have real $\prod$-types, we can't bring those
invariants into our program specifications in the same way a
dependently-typed language could. But with GHC 7.4's new `-XDataKinds`
extension, many data types are automatically promoted into the
kind-level; this means that we can maintain a static-dynamic phase
distinction at the same time as having data structures mirrored into the
kind level. Combined with phantom-types, this is enough to start
encoding interesting invariants into the type system. **But, reusing
statically verified code for dynamic values can be problematic.**

 <!--more-->

Let's look back at our length-indexed vectors to see the problem
first-hand:

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE TypeFamilies #-}

> module PhasedConstraints where
> import Control.Applicative ((<$>))

> data Nat = Z | S Nat
> infixr :>
> data Vect :: * -> Nat -> * where
>   VNil :: Vect a Z
>   (:>) :: a -> Vect a n -> Vect a (S n)
> deriving instance Show a => Show (Vect a n)

It's trivial to write safe `head` and `tail` functions now:

< vhead :: Vect a (S n) -> a
< vhead (x :> xs) = x

< vtail :: Vect a (S n) -> Vect a n
< vtail (x :> xs) = xs

But what if we don't statically know the size of our vector? For
instance, if we are converting a plain old list to a vector, or if
we are parsing some file. We'd presumably box up the vector and
existentially quantify its length:

> data EVect :: * -> * where
>   EVect :: Vect a n -> EVect a
> deriving instance Show a => Show (EVect a)

Now we can marshall $[\alpha]$ into $\forall (\alpha:Set). \exists
(n:\mathbb{N}). a^n$:

> fromList :: [a] -> EVect a
> fromList [] = EVect VNil
> fromList (x:xs) =
>   case fromList xs of
>     EVect v -> EVect $ x :> v

But now, we're in a bit of a pickle, since we cannot reuse our `vhead`
and `vtail`.  So, we can just write new ones that target `Maybe`:

< evhead :: EVect a -> Maybe a
< evhead (EVect VNil)     = Nothing
< evhead (EVect (a :> b)) = Just a

< evtail :: EVect a -> Maybe (EVect a)
< evtail (EVect VNil)     = Nothing
< evtail (EVect (a :> b)) = Just (EVect b)

But this is certainly less-than-desirable. We haven't reused any of our
code. Imagine how unfortunate this would be for a function that is more
complicated than `head` or `tail`... What we actually need is a way to
marshall a function with static guarantees to a function with dynamic
guarantees without rewriting everything, and without case analysis.

<h3>Types as Propositions</h3>

What if instead of restricting the type of the input of `vhead`, we just
required a proof that it was a nonempty vector? Let's try that. Under
the Curry-Howard correspondance, types are propositions, and values are
proofs. So, we can create a type that represents the proposition
*greater-than-zero* for natural numbers:

> data NotZero :: Nat -> * where
>   NotZero :: NotZero (S n)
> deriving instance Show (NotZero n)

Basically, excluding *bottom*, `NotZero n` does not have any inhabitants
of type `NotZero Z`. So, forall natural numbers `n`, any value `NotZero
n` is a proof that `n` is not zero. And we can just take this as a
parameter in our new version of `vhead`:

> vhead :: Vect a n -> NotZero n -> a
> vhead (x :> xs) NotZero = x

To use `vhead`, just include the proof in the parameters:

< exampleVector = "hello" :> "world" :> VNil
< hello = vhead exampleVector NotZero
< -- typeError = vhead VNil NotZero

It's a bit more complicated for `vtail`, since the length of its input
must be universally quantified for it to be able to be applied to a
vector with existentially quantified length; that is, behavior must not
depend on the value of the index, since the index will just be a skolem
type variable in the unpacked existential. So, we need to do subtraction
in the right-hand side of the arrow, rather than structural recursion on
the left side. To do this, we can use a type family:

> type family Prev (n :: Nat) :: Nat
> type instance Prev (S n) = n

> vtail :: Vect a n -> NotZero n -> Vect a (Prev n)
> vtail (x :> xs) NotZero = xs

Now, in order to make this work for vectors of unknown length, we need a
function that can generate a `NotZero` proof for all vectors if
applicable:

> notEmpty :: Vect a n -> Maybe (NotZero n)
> notEmpty VNil = Nothing
> notEmpty (x :> xs) = Just NotZero

And now, implementing `evhead` and `evtail` in terms of the originals is
trivial, thanks to currying:

> evhead :: EVect a -> Maybe a
> evhead (EVect v) = vhead v <$> notEmpty v

> evtail :: EVect a -> Maybe (EVect a)
> evtail (EVect v) = EVect . vtail v <$> notEmpty v

Gloriously, all the pattern matching and so forth is factored out into
the proof generator. This is especially helpful if you have a whole
family of functions that you want to be available to both your
statically indexed values, and those whose indices are only known
dynamically.

<h3>Taking a breath.</h3>

The reason we even bothered to do this is that there are more instances
of statically verifiable computation in our programs than we like to
think. For instance, any time we use a literal form for a data
structure, rather than building it up inductively from IO, we are doing
something that might benefit from static verification.

But in the popular literature on dependent types (and
faking-dependent-types-with-GADTs), there's quite a bit of focus on
building machinery to facilitate this static verification, but not a lot
of details on how to make that machinery useful for dynamic data. By
factoring constraints into explicit proofs, we can allow our constraints
to be checked in different program phases (compilation or execution),
according to the requirements of our data (static or dynamic). In a
sense, we get to have our cake, and eat it too.
