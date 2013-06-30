---
title: Static and Dynamic Proof Inference in Haskell
---

Previously, I
[discussed](http://www.jonmsterling.com/posts/2012-07-22-flexibly-phased-constraints-in-haskell.html)
using explicit proof parameters in combination with lifted proof
generators in order to write code that can distribute its invariants
across static and dynamic phases as appropriate. The major disadvantage
to that approach is that it can sometimes be difficult and cumbersome to
create proofs of certain properties manually. Today, I'll discuss how to
automate that using type classes.

 <!--more-->

Before we begin, I'd like to further address the question of whether we
even need to use explicit proof terms, as it would seem that we could
simply use type classes to express our constraints. Let's try that with
a mirroring vector (an collection of an indexed type, where its value is
mirrored in both the value and type levels). First, the usual
boilerplate:

> {-# LANGUAGE GADTs, KindSignatures #-}
> {-# LANGUAGE DataKinds, PolyKinds #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverlappingInstances #-}

> import Control.Applicative

For some indexed type `f i`, there is a vector parameterized by a
type-level list of `i`, holding a linked list of values `f i`.

> infixr :<
> data Vect f is where
>   VNil :: Vect f '[]
>   (:<) :: f i -> Vect f is-> Vect f (i ': is)

An example indexed type for the vector might be a natural number GADT:

> data Nat = Z | S Nat
> data DNat (n :: Nat) where
>   DZ :: DNat Z
>   DS :: DNat n -> DNat (S n)

> natToNum :: Num a => DNat n -> a
> natToNum DZ = 0
> natToNum (DS n) = 1 + natToNum n

> instance Show (DNat n) where
>   show = show . natToNum

We wish to have a function to get the index of an item that's contained
in the vector. For that to be safe, we need to be able to prove that the
item is even contained in the vector.

<h3>An attempt without explicit witnesses</h3>

So, we'll first try just using a type class. Since the values are
guaranteed to be mirrored by the type-list, this is a reasonable way to
do it:

< class Elem' i is
< instance Elem' i (i ': is)
< instance Elem' i is => Elem' i (j ': is)

But you'll quickly find that writing a function which uses this
constraint is not a simple proposition. In fact, it would appear to be
impossible for more than one reason. If we wish to use type classes in
this way, we shall actually have to write an `IndexOf` type class:

< class IndexOf i is where
<   indexOf :: Num a => Vect f is -> f i -> a
< instance IndexOf i (i ': is) where
<   indexOf (i :< is) i' = 0
< instance IndexOf i is => IndexOf i (j ': is) where
<   indexOf (j :< is) i = 1 + indexOf is i

And that class will do what we expect, but it **complects constraint
with behavior**. This is a "bad" thing, since there may be other
functions that require a similar constraint but quite different
behavior. That smells of poor factoring. And so we must return to our
explicit proof witness strategy.


<h3>An attempt with explicit witnesses</h3>

A witness that a value is an item of our mirrored vector is an instance
of an inductive family. The base case is that the element lies at the
beginning; if we have a proof that it lies somewhere in the vector's
tail, we also have a proof that the it is an element of the vector as a
whole:

> data Elem a as where
>   Here :: Elem x (x ': xs)
>   There :: Elem x xs -> Elem x (y ': xs)

And so we implement `indexOf` by recursing through both the values _and_
the layers of proofs!

> indexOf :: Num a => Vect f is -> f i -> Elem i is -> a
> indexOf (x :< xs) x' Here = 0
> indexOf (y :< xs) x (There p) = 1 + indexOf xs x p

But, of course, this is absolutely useless at a glance, since in order
to even use the function, the programmer has to type a chain of proofs
isomorphic to the Peano encoding of the item's index anyway!

> testVect :: Vect DNat [Z, S (S Z), S Z]
> testVect = DZ :< (DS (DS DZ)) :< (DS DZ) :< VNil
> noBueno = indexOf testVect (DS (DS DZ)) (There Here) -- 1

So, this is a total non-starter unless we can generate the proof term.
Since only one term is needed to prove a proposition, we can make a very
simple type class for proof inference:

> class Infer p where
>   infer :: p

And we can directly mirror the `Elem` family (but in the opposite
direction):

> instance Infer (Elem x (x ': xs)) where
>   infer = Here
> instance Infer (Elem x xs) => Infer (Elem x (y ': xs)) where
>   infer = There infer

It's trivial to write and use a variant of `indexOf` which uses our new
proof inference class:

> indexOf' xs x = indexOf xs x infer
> muchoBueno = testVect `indexOf'` (DS (DS DZ)) -- 1


<h4>Another example: safe vector access</h4>

To access a vector at a given index, we need to make sure it's within
bounds. So, we first provide a way to get the length of a type-list:

> type family Length xs :: Nat
> type instance Length '[] = Z
> type instance Length (x ': xs) = S (Length xs)

We also need to provide a way to get an element from a type list using
an index:

> type family At xs n :: Nat
> type instance At (x ': xs) Z = x
> type instance At (x ': xs) (S n) = At xs n

And now we need a type of less-than proofs:

> data LT (l :: Nat) (r :: Nat) where
>   ZLT :: LT Z (S n)
>   SLT :: LT n m -> LT (S n) (S m)

The way we go about `at` is much the same as in the previous example,
just a tiny bit more complicated in the types.

> at :: Vect f is -> DNat n -> LT n (Length is) -> f (At is n)
> at (x :< xs) DZ ZLT = x
> at (y :< xs) (DS n) (SLT lt) = at xs n lt

> instance Infer (LT Z (S n)) where
>   infer = ZLT

> instance Infer (LT n m) => Infer (LT (S n) (S m)) where
>   infer = SLT infer

> v @. i = at v i infer

> hailMary = testVect @. (DS DZ) -- 2


<h3>Inferring dynamic proofs</h3>

Dynamic proof inference is the same as static proof inference, except
that it may fail:

> class DynamicInfer p where
>   inferDynamic :: Maybe p

We have to go through this little dance for each proof which we want to
make available dynamically. The routine is repetitive enough that one
could conceivably use Template Haskell to factor it out, though I'd
certainly be pleased to learn if there was a more clean and succinct way
of doing it.

> instance DynamicInfer (Elem x (x ': xs)) where
>   inferDynamic = Just infer
> instance DynamicInfer (Elem x xs) => DynamicInfer (Elem x (y ': xs)) where
>   inferDynamic = There <$> inferDynamic
> instance DynamicInfer (Elem x ys) where
>   inferDynamic = Nothing

> instance DynamicInfer (LT Z (S n)) where
>   inferDynamic = Just infer
> instance DynamicInfer (LT n m) => DynamicInfer (LT (S n) (S m)) where
>   inferDynamic = SLT <$> inferDynamic
> instance DynamicInfer (LT n m) where
>   inferDynamic = Nothing

And now it is trivial to make dynamic versions of our functions:

> dynamicIndexOf :: Num a => Vect f is -> f i -> Maybe a
> dynamicIndexOf xs x = indexOf xs x <$> inferDynamic

> dynamicAt :: DynamicInfer (LT n (Length is)) => Vect f is -> DNat n -> Maybe (f (At is n))
> dynamicAt xs i = at xs i <$> inferDynamic
