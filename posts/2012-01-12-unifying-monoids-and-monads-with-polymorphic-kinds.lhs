---
title: Unifying Monoids and Monads with Polymorphic Kinds
---

<blockquote>A monad is just a monoid in the category of endofunctors,
what's the problem? --- [James
Iry](http://james-iry.blogspot.com/2009/05/brief-incomplete-and-mostly-wrong.html).</blockquote>

Building on the work of sigfpe's [From Monoids to
Monads](http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html) and
monoidal's [Kind polymorphism in
action](http://monoidal.blogspot.com/2010/07/kind-polymorphism-in-action.html)
(which demonstrates kind polymorphism in the [Ultrecht Haskell
Compiler](http://www.cs.uu.nl/wiki/UHC)), we can unify Monoid and Monad
under one type class in GHC 7.4.

 <!--more-->

So, the quote at the beginning is true, but with a few qualifications.
As most beginning functional programmers know it, the monoid is a
structure that has an identity element $id$ and a binary operator
$\otimes$:

$\begin{aligned}
\text{class} &\textbf{Monoid}\ (m : \star) \text{ where}\\
& id: m\\
& \otimes: m \to m \to m
\end{aligned}$

For the monoid $m$ to be valid, the $id$ must be the identity with
respect to the operator $\otimes$, like $0$  is to $+$ on natural
numbers, or $1$ to $\times$, etc; the binary operator must also be
associative:

$\begin{aligned}
\forall x &: m.\ &x\otimes id &\equiv id\otimes x \equiv x\\
\forall a,b,c &: m.\ &(a\otimes b)\otimes c &\equiv a\otimes (b\otimes c)
\end{aligned}$

This kind of monoid does not bear the sort of abstraction required to
unify it with Monad, which is rather different:

$\begin{aligned}
\text{class} &\textbf{Functor}\ m \Rightarrow \textbf{Monad}\ (m : \star\to\star) \text{ where}\\
& \eta: \forall\alpha:\star. (\textbf{Id}\ \alpha \to m\,\alpha) \\
& \mu: \forall\alpha:\star. (m\, (m\,\alpha) \to m\,\alpha)
\end{aligned}$

In fact, $\eta$ is a natural transformation[^nat] from the Identity
functor to another functor $m$; $\mu$ is a natural transformation from
$m^2$ to $m$ (that is, from $m$ applied twice to $m$ applied once).

[^nat]: A *natural transformation* is an arrow from one functor to another.


### A Difference of Kinds

You'll note that our $\textbf{Monad}$ and $\textbf{Monoid}$ operate in
totally different worlds: a difference of kinds. That is, an instance of
the former is a type of values ($m :\star$), whereas an instance of the
latter is an arrow from one type to another (a type constructor, $m
:\star\to\star$). By analogy, then, the former's functions should be
natural transformations in the latter.

This makes fine sense, but the question of what to do with $id$ remains:
why does it have an input in $\textbf{Monad}$, but not in
$\textbf{Monoid}$? If we are going to understand these functions as
arrows between objects in a category, then $id$ must have an input. As a
natural transformation in $\textbf{Monad}$, its input is the identity
functor; as a simple function in $\textbf{Monoid}$, its input should be
$\varnothing$, nothing.

If we uncurry $\otimes$, then its type becomes ($m\times m\to m$); this
adjustment brings its type in line with that of $\mu$. So, we can build
the following (incomplete and flawed) generalization over an identity
element $id$ and a type-operator $\times$:

$\begin{align}
\text{class} &\textbf{Monoid}\ (id: k)\ (\times : k\to k\to \star)\ (m : k)\ \text{ where}\\
& id: id \to m\\
& \otimes: m\times m \to m
\end{align}$

$\begin{align}
\text{instance} &\textbf{Num}\ \alpha \Rightarrow \textbf{Monoid}\ \varnothing\ (\Lambda x\,y. (x,y))\ \alpha\ \text{ where}\\
& id = const\ 0\\
& \otimes = uncurry\ (+)
\end{align}$

$\begin{align}
\text{instance} &\textbf{Monoid}\ \textbf{Id}\ (\Lambda f\,g\,\alpha. f\ (g\,\alpha))\ [\,]\ \text{ where}\\
& id\ (\textbf{Id}\ x) = [x]\\
& \otimes [xs] = xs
\end{align}$

But this doesn't work, since the type of $id$ for our second (monadic)
instance reduces to $Id\to []$, which doesn't type-check: that is, the
kind of $(\to)$ is $\star\to\star\to\star$, but the kind of each of its
operands in $id$ is already ($\star\to\star$). What this means is that
for monadic instances of $\textbf{Monoid}$, we need an arrow type
constructor of kind $(\star\to\star)\to(\star\to\star)\to\star$. So we
need to abstract over kind of arrow constructor:

$\begin{align}
\text{class} &\textbf{Monoid}\ (\leadsto\;: k\to k\to\star)\ (id: k)\ (\times : k\to k\to \star)\ (m : k)\ \text{ where}\\
& id: id \leadsto m\\
& \otimes: m\times m \leadsto m
\end{align}$

A normal monoid deals with function arrows and pairs:

$\begin{align}
\text{instance} &\textbf{Num}\ \alpha \Rightarrow \textbf{Monoid}\ (\to)\ \varnothing\ (\Lambda x\,y. (x,y))\ \alpha\ \text{ where}\\
& id = const\ 0\\
& \otimes = uncurry\ (+)
\end{align}$

A monadic monoid deals with natural transformations and composed
functors:

$\begin{align}
\text{instance} &\textbf{Monoid}\ (\Lambda f\,g\,\alpha. f\,\alpha\to g\,\alpha)\ \textbf{Id}\ (\Lambda f\,g\,\alpha. f\ (g\,\alpha))\ [\,]\ \text{ where}\\
& id\ (\textbf{Id}\ x) = [x]\\
& \otimes [xs] = xs
\end{align}$

### The Haskell Version

We'll need to turn on a bunch of GHC extensions.

> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE UnicodeSyntax #-}

> module GeneralizedMonoid where
> import Control.Monad (Monad(..))
> import Data.Monoid (Monoid(..))

First we define the type class `Monoidy`:

> class Monoidy (~>) comp id m | m (~>) → comp id where
>   munit :: id ~> m
>   mjoin :: m `comp` m ~> m

We use functional dependencies to help the typechecker understand that
`m` and `~>` uniquely determine `comp` ($times$) and `id`.

This kind of type class would not have been possible in previous
versions of GHC; with the new kind system, however, we can abstract over
kinds![^polykinds] Now, let's create types for the additive and
multiplicative monoids over the natural numbers:

[^polykinds]: *Caveat*: you cannot use kind variables in annotations
(like `m :: k`, as you can with type variables). Hopefully this will be
fixed soon.

> newtype Sum a = Sum a deriving Show
> newtype Product a = Product a deriving Show
> instance Num a ⇒ Monoidy (→) (,) () (Sum a) where
>   munit _ = Sum 0
>   mjoin (Sum x, Sum y) = Sum $ x + y
> instance Num a ⇒ Monoidy (→) (,) () (Product a) where
>   munit _ = Product 1
>   mjoin (Product x, Product y) = Product $ x * y

It will be slightly more complicated to make a monadic instance with
`Monoidy`. First, we need to define the identity functor, a type for
natural transformations, and a type for functor composition:

> newtype Id α = Id { runId :: α } deriving Functor

A natural transformation ($\Lambda f\,g\,\alpha.
(f\,\alpha)\to(g\,\alpha)$) may be encoded in Haskell as follows:

> newtype NT f g = NT { runNT :: ∀ α. f α → g α }

Functor composition ($\Lambda f\,g\,\alpha. f\ (g\,\alpha)$) is encoded
as follows:

> newtype FC f g α = FC { runFC :: f (g α) }

Now, let us define some type `T` which should be a monad:

> newtype Wrapper a = Wrapper { runWrapper :: a } deriving (Show, Functor)
> instance Monoidy NT FC Id Wrapper where
>   munit = NT $ Wrapper . runId
>   mjoin = NT $ runWrapper . runFC

With these defined, we can use them as follows:

< ghci> mjoin (munit (), Sum 2)
<       Sum 2
< ghci> mjoin (Product 2, Product 3)
<       Product 6
< ghci> runNT mjoin $ FC $ Wrapper (Wrapper "hello, world")
<       Wrapper {runWrapper = "hello, world" }

We can even provide a special binary operator for the appropriate
monoids as follows:

> (<+>) :: Monoidy (→) (,) () m ⇒ m → m → m
> (<+>) = curry mjoin

< ghci> Sum 1 <+> Sum 2 <+> Sum 4
<       Sum 7

Now, all the extra wrapping that Haskell requires for encoding this is
rather cumbersome in actual use. So, we can give traditional `Monad` and
`Monoid` instances for instances of `Monoidy`:

> instance Monoidy (→) (,) () m ⇒ Monoid m where
>   mempty = munit ()
>   mappend = curry mjoin

> instance (Functor m, Monoidy NT FC Id m) ⇒ Monad m where
>   return x = runNT munit $ Id x
>   x >>= f = runNT mjoin $ FC (f `fmap` x)

And so the following works:

< ghci> mappend mempty (Sum 2)
<       Sum 2
< ghci> mappend (Product 2) (Product 3)
<       Product 6
< ghci> join $ Wrapper $ Wrapper "hello"
<       Wrapper {runWrapper = "hello" }
< ghci> Wrapper "hello, world" >>= return
<       Wrapper {runWrapper = "hello, world" }


### If you got this far...

I hope you enjoyed that! I can't express enough my thanks to the people
who came before me and helped me indirectly to refine my ideas and
understanding of the relationship between monads and monoids.
Additionally, a shout-out to the GHC team for adding kind polymorphism!
