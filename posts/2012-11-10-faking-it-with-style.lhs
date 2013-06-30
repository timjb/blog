---
title: Dependent Types Today: Faking It With Style
---

There have been various attempts at faking dependent types in Haskell,
most notably Conor McBride's [Strathclyde Haskell
Enhancement](https://personal.cis.strath.ac.uk/~conor/pub/she/). Since
its creation, several improvements to GHC have set the stage for some of
SHE's features to be implemented natively.

Let's see what's necessary to fake dependent types with singletons in
today's GHC.

 <!--more-->

First, the usual boilerplate:

> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE PolyKinds, DataKinds #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
> {-# LANGUAGE UndecidableInstances, IncoherentInstances #-}
> {-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
> {-# LANGUAGE ViewPatterns, UnicodeSyntax #-}
> {-# LANGUAGE RankNTypes, GADTs #-}

> module FakingIt where

> import Prelude hiding (lookup)
> import GHC.TypeLits

Now, in order to simulate dependent types, we need to allow data to
exist in three forms:

1. Terms under types (we get this for free when we define data
types).

2. Terms under kinds (we also get this for free when `-XDataKinds` is
turned on).

3. Values that introduce indexed types (we must manually create or
generate a GADT encoding of singleton sets). This is a sort of glue
between the first two encodings.

Let's see how this works in practice! We'll start, as always, with a
type of dogs:

> data Doggy = Tucker | Rover

We have introduced a type `Doggy` inhabited by two terms at the
value-level; we have also introduced a kind `'Doggy` inhabited by two
terms at the type level. Now, we could create the "glue" GADT as
follows:

< data SDoggy :: Doggy → * where
<   STucker :: SDoggy Tucker
<   SRover  :: SDoggy Rover

But GHC provides a `Sing` type family under which we should create our
singleton sets; this allows us to use some built-in machinery, like
`sing` which allows one to infer the right singleton value for a known
type. So, we'll do the following:

> data instance Sing (d :: Doggy) where
>   STucker :: Sing Tucker
>   SRover  :: Sing Rover

We'll need to help out with the inference instances:

> instance SingI Tucker where sing = STucker
> instance SingI Rover  where sing = SRover

And finally, we need to provide conversion from the singleton type to
the plain-old-value type:

> instance SingE (Kind :: Doggy) Doggy where
>   fromSing STucker = Tucker
>   fromSing SRover  = Rover

It's clear what this does, but it might not be immediately obvious what
the `(Kind :: Doggy)` business is. Basically, Haskell does not currently
allow one to abstract over kinds in a straight-forward way. Since we're
basically trying to make a map from kinds to types (that is, from the
kind which `-XDataKinds` generated to the type whence it arose), we need
to pass the kind in as the annotation to a dummy type; in this case, we
use `Kind`, which is provided by `GHC.TypeLits`, and has no actual
meaning by itself.

As you can see, all this boilerplate could in fact be generated with
Template Haskell. In fact, the
[Singletons](http://hackage.haskell.org/package/singletons) package
already does so, but it does not use GHC's built-in kit, so there's a
bit of unhappiness there.

 <blockquote>
**Update**: The Singletons package has been updated, and is now compatible with
TypeLits.
 </blockquote>


Singletons for Higher Order Types
---------------------------------

Before we get started, now would be a good time to introduce some
naughty little shorthands that will make our code a bit less noisy:

> type Π = Sing
> π = fromSing

The capital `Π` adds a bit of swagger to our dependent function types.
Combined with `-XViewPatterns`, `π` lets us pattern match on singleton
parameters.

The procedure for higher order types (like lists, pairs, etc.) is
basically the same as the above. Just a bit more involved; we'll start
with lists:

> data instance Sing (xs :: [k]) where
>   SNil :: Sing '[]
>   SCons :: Π (x :: k) → Π (xs :: [k]) → Sing (s ': xs)

> instance SingI '[] where sing = SNil
> instance (SingI x, SingI xs) ⇒ SingI (x ': xs) where
>   sing = SCons (sing :: Sing x) (sing :: Sing xs)

> instance SingE (Kind :: k) r ⇒ SingE (Kind :: [k]) [r] where
>   fromSing SNil = []
>   fromSing (SCons x xs) = fromSing x : fromSing xs

> data instance Sing (p :: (a,b)) where
>   SPair :: Sing a → Sing b → Sing '(a,b)

> instance (SingE (Kind :: a) x, SingE (Kind :: b) y) ⇒
>          SingE (Kind :: (a,b)) (x,y) where
>   fromSing (SPair x y) = (fromSing x, fromSing y)
> instance (SingI k, SingI v) ⇒ SingI '(k,v) where
>   sing = SPair sing sing

Sigma Types: Yes, We Can (to a point)
-------------------------------------

Consider the following definition, adapted from `Data.Products` in the
Agda standard library:

< record Σ {a b} (A : Set a) (B : A → Set b) : Set (a ⊔ b) where
<   construct _,_
<   field
<     proj₁ : A
<     proj₂ : B proj₁

This is the classic definition of a [dependent
sum](http://www.jonmsterling.com/posts/2012-09-07-pi-is-for-power-sigma-for-product.html)
in Agda. Basically, the first field is a type of some sort, and the
second field is a proof that that type satisfies some predicate. Minus
the record accessor sugar, the constructor `_,_` comes out as the
following:

< data Σ {a b} (A : Set a) (B : A → Set b) : Set (a ⊔ b) where
<   _,_ : (proj₁ : A) → B proj₁ → Σ A B

We would hope to be able to create something analogous in Haskell under
the limited encoding of dependent types possible using singletons.

Now, as you can see, the Agda version is very flexible in terms of the
sorts of things it can quantify, because of its use of [universe
polymorphism](http://www.jonmsterling.com/posts/2012-09-08-adding-universe-polymorphism.html).
Now, we can't hope for anything quite as profound in Haskell; we'll
settle for having the second parameter be a predicate over terms of some
kind, which is supplied in the first parameter.

We can now create the following definition:

< data Σ (kind :: k) (pred :: k → *) where
<   (:|-) :: Π (x :: k) → pred x → Σ kind pred

Note that this is equivalent to the following (minus our little `Π`
shorthand which we use in the spirit of dependent types):

< data Σ (kind :: k) (pred :: k → *) where
<   (:|-) :: Sing x → pred x → Σ kind pred

Last, we put the final touches on by making this into a record:

> data Σ (kind :: k) (pred :: k → *) where
>   (:|-) :: { proj₁ :: Π (x :: k), proj₂ :: pred x } → Σ kind pred

Since in most cases, the kind over which the predicate operates is
inferrable from the predicate itself, we provide the following
convenience synonym:

> type Exists (pred :: k → *) = Σ Kind pred

So, we could, for instance, make a function that takes a number which
is not one:

> data NotOne :: Nat → * where
>   NotOneZ :: NotOne 0
>   NotOneSS :: NotOne (n + 2)

> cantTakeOne :: Fractional t ⇒ Exists NotOne → t
> cantTakeOne ((π → n) :|- _) = 10.0 / (fromInteger n - 1)

We can also show that our Haskell encoding really is similar to the Agda
version by providing our own version of the encoding of cartesian
products (remember, a cartesian product arises when the predicate
ignores its parameter).

If it were possible to have type-level lambdas in Haskell, then this
would be just as simple as in Agda:

< type x & y = Σ x (λ _ → y)

Unfortunately, we must actually provide a little bit of indirection to
get around this limitation:

> type x & y = Σ x (Const y)
> data Const :: l → k → * where
>   Const :: Π (x :: l) → Const (Kind :: l) y

As such, to hide away the `Const` indirection, we'll need to provide our
own constructors and eliminators for products:

> (&) :: Π (x :: l) → Π (y :: k) → (Kind :: l) & (Kind :: k)
> x & y = x :|- Const y

> prodElim :: SingI pair ⇒ (Kind :: l) & (Kind :: k) → Sing (pair :: (l,k))
> prodElim _ = sing

Looks like our kit should be sufficient to make a pair of dogs:

> dawgFriends = STucker & SRover


<h3>A dependent head function over lists</h3>

There are several common ways to make a safe `head` function, many of
which I have discussed in the past. Some of these involve having the
function operate on lists which are non-empty by construction (using a
length-indexed vector, or a list type which has no nil). If our list,
however, exists at both the value and the type levels, we can just
predicate over it without changing the structure of the list itself.
Consider the following `NonEmpty` predicate:

> data NonEmpty :: [k] → * where
>   NonEmpty :: NonEmpty (x ': xs)

We can provide this as the predicate to our dependent sum!

> safeHead :: SingE (Kind :: k) r ⇒ Σ (Kind :: [k]) NonEmpty → r
> safeHead ((π → x : _) :|- _) = x

Note that we used a combination of `-XViewPatterns` and our little `π`
shorthand to pattern match on the value-level reflection of the list.
This definition is equivalent to the following:

< safeHead ((SCons x xs) :|- _) = fromSing x


<h3>Safe Dictionary Lookup</h3>

We can do much the same thing with dictionaries:

> data HasKey :: l → [(l,k)] → * where
>   KeyHere  :: HasKey k ('(k, v) ': dict)
>   KeyThere :: HasKey k dict → HasKey k (pair ': dict)

> lookup :: (SingE (Kind :: k) k', SingE (Kind :: v) v') ⇒
>           Π (key :: k) →
>           Σ (Kind :: [(k,v)]) (HasKey key) → v'
> lookup _ ((π → (_,x) : _) :|- KeyHere)     = x
> lookup k (SCons _ xs      :|- KeyThere hk) = lookup k (xs :|- hk)


Of course, we can use the same trick as we always do by providing
implicit proof terms:

> class Implicit x where
>   implicitly :: x
> instance Implicit (HasKey k ('(k,v) ': dict)) where
>   implicitly = KeyHere
> instance Implicit (HasKey k dict) ⇒ Implicit (HasKey k (pair ': dict)) where
>   implicitly = KeyThere implicitly

> lookup' k dict = lookup k (dict :|- implicitly)

> type DogNamesMap = ['(Tucker, "Tucker"), '(Rover, "Rover")]
> sDogNamesMap = sing :: Sing DogNamesMap

> tuckersName = lookup STucker (sDogNamesMap :|- KeyHere)
> tuckersName' = lookup' STucker sDogNamesMap

Caveats
-------

That was fun! But this was mostly a demonstration to show that Σ-types
are possible in Haskell, rather than a recommendation to use them. For
one, it is a fair bit more complicated under this system to allow such
functions to operate on dynamic data (which may or may not satisfy the
static predicate), than under the system I proposed in [my previous
post](http://www.jonmsterling.com/posts/2012-08-05-static-and-dynamic-proof-inference-in-haskell.html).

Another thing which makes the singleton-encoding of dependent types
rather unpleasant to use is the fact that whilst we can pretty easily
reflect data up and down the ladder of universes, it's rather more
difficult to do the same with functions in a general way. So (at least
today), you would have to write all your functions over such data as
type families, and then reflect that down to the value level. This is
probably possible, but it is indeed quite a lot to ask.

I'm hoping that the situation will continue to improve, though! GHC 7.6
finally made it possible (though not particularly pleasant) to do
non-trivial kind abstraction. It is of great interest to me to see what
is improved next.

Thanks for playing along!
