---
title: Expressing Church Pairs with Types
---

It's easy to express pairs in the untyped lambda calculus; adding types
makes it somewhat more complex to get a correct encoding.  In this
article, I explain how we can build a pair with typesafe accessors in a
polymorphically typed lambda calculus with type operators (System
$F_\omega$), and demonstrate it in Haskell. This document is Literate
Haskell: simply copy and paste it into an `.lhs` file, and you can feed
it into GHCI.

 <!--more-->

In the untyped lambda calculus, a pair $(x,y)$ is encoded as a
function which takes two values $x$, $y$, yielding a new function that
takes an accessor function $f$ as its argument (that is, $f$ selects
either the left or the right argument):

$\begin{aligned}
\textbf{pair} &:= \lambda xyf. f\,xy\\
\textbf{fst}  &:= \lambda p. p\, (\lambda xy. x)\\
\textbf{snd}  &:= \lambda p. p\, (\lambda xy. y)
\end{aligned}$

So, the following reductions can occur:

$\begin{aligned}
\textbf{pair}\; ab &\equiv (\lambda xyf. f\,xy)\; ab\\
                   &\to \lambda f. f\,ab\\
\textbf{fst}\; (\textbf{pair}\; ab) &\equiv \textbf{fst}\; (\lambda f. f\,ab)\\
&\to (\lambda p. p\, (\lambda xy. x))\; (\lambda f. f\,ab)\\
&\to (\lambda f. f\,ab)\; (\lambda xy. x)\\
&\to (\lambda xy. x)\; ab\\
&\to a\\
\textbf{snd}\; (\textbf{pair}\; ab) &\equiv \textbf{snd}\; (\lambda f. f\,ab)\\
&\to (\lambda p. p\, (\lambda xy. y))\; (\lambda f. f\,ab)\\
&\to (\lambda f. f\,ab)\; (\lambda xy. y)\\
&\to (\lambda xy. y)\; ab\\
&\to b\\
\end{aligned}$

Translating our untyped example into the typed lambda calculus naïvely
doesn't change much of anything. The main features of this new calculus
are polymorphic types ($\alpha,\beta,\gamma$...), and type-level functions
corresponding to universal quantification. Types are abstracted over using a capital
lambda.

$\begin{aligned}
\textbf{pair} &:= \Lambda\alpha\beta\gamma. \lambda (x : \alpha) (y : \beta)
(f : \alpha\to\beta\to\gamma). f\,xy\\
\textbf{fst} &:= \Lambda\alpha\beta\gamma. \lambda
(p : {(\alpha\to\beta\to\alpha)\to\gamma}). p (\lambda (x : \alpha) (y : \beta).
x)\\
\textbf{snd} &:= \Lambda\alpha\beta\gamma. \lambda
(p : {(\alpha\to\beta\to\beta)\to\gamma}). p (\lambda (x:\alpha) (y:\beta). y)
\end{aligned}$

However, this is problematic. What this definition of $\textbf{pair}$
ends up saying is that *for all* types $\alpha,\beta,\gamma$, there is a
function that will retrieve a value in $\gamma$ from a pair in
$(\alpha,\beta)$. This, however, is only true inasmuch as $\gamma$ is
either $\alpha$ or $\beta$. Thus, our current definition will allow more
incorrect programs than is acceptable in a typed (but not
dependently-typed) language. What we really need is to constrain
$\gamma$:

$\begin{aligned}
\textbf{pair} &:= \Lambda\alpha\beta. \lambda (x:\alpha) (y:\beta)
(f:{\alpha\to\beta\to(\alpha\lor\beta)}). f\,xy\\
\textbf{fst} &:= \Lambda\alpha\beta. \lambda
(p:{(\alpha\to\beta\to\alpha)\to\alpha}). p (\lambda (x:\alpha)(y:\beta).
x)\\
\textbf{snd} &:= \Lambda\alpha\beta. \lambda
(p:{(\alpha\to\beta\to\beta)\to\beta}). p (\lambda (x:\alpha)(y:\beta). y)
\end{aligned}$

So, we have created a typed encoding of the Church Pair which only allows
accessors which return a value of the correct type.


Encoding our typed Church Pair in Haskell
==========================================

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE Rank2Types #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE IncoherentInstances #-}
> {-# LANGUAGE UnicodeSyntax #-}

> module ChurchPair where
> import Prelude hiding (fst, snd)

The first order of business will be to implement the disjunction
constraint ($\Lambda\alpha\beta. \alpha\lor\beta$) in Haskell. To do
this, we can use a type class:

> class TypeOr α β γ

and inhabit it with the proper combinations:

> instance TypeOr α β α
> instance TypeOr α β β

Now, we can create a type synonym (for the sake of convenience) that
encapsulates the structure of pairs:

> type Pair α β = ∀ γ. TypeOr α β γ ⇒ (α → β → γ) → γ

A $\textbf{Pair}$ over $(\alpha,\beta)$ is function that takes a
function that returns one of its elements. Note that we cannot specify
within this non-dependent type system that the result of this accessor actually be one
of the elements, but the `(TypeOr α β γ)` constraint allows us to at least
ensure that the result is of the type of either element in the pair.

And now, given two values, we can construct a $\textbf{Pair}$:

> pair :: α → β → Pair α β
> pair x y f = f x y

The accessors are easily written, and their type annotations make clear
their function:

> fst :: Pair α β → α
> fst p = p $ \x y → x

> snd :: Pair α β → β
> snd p = p $ \x y → y

Incidentally, `fst` and `snd` can also be defined in terms of library
functions:

> fst' p = p const
> snd' p = p $ flip const

To demonstrate the benefit our typing constraints, try the following:

< badAccessor :: Pair α β → String
< badAccessor p = p $ \x y → "Hello" -- type error

It is impossible to create an accessor of the wrong type (unless the
accessor itself is the *bottom*, in which case you will have a runtime
error).
