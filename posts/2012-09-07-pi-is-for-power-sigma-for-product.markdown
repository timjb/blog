---
title: Pi is for Power, Sigma for Product
---

$\prod$ and $\sum$ are two fundamental connectives of [Intuitionistic
Type Theory](http://en.wikipedia.org/wiki/Intuitionistic_type_theory),
corresponding to $\forall$ and $\exists$ in logic respectively. In their
non-dependent special cases, they represent cartesian powers and
products respectively. Let's take a look at how this works!

<!--more-->

## $\prod$ generalizes arrows, $\sum$ generalizes products

Anyone who took elementary mathematics is probably aware that $\prod$ is
the mathematical symbol for a product of a sequence; you've probably
seen something like this:
$$\begin{align}
    \prod_{i\in\mathbb{N}} f(i)
\end{align}$$, the product of $f(i)$ over the set of all natural numbers
$\mathbb{N}$.

The word "product" gets us mistakenly thinking that a $\prod$-type
should be a pair or something, like `(,)` in Haskell. Consider the
following function type for making a vector of '!'-characters of a given
length in an Agda-like language with dependent types:

~~~~{.Haskell}
replicate : (n:ℕ) → Vect n Char -- syntactic sugar
replicate : Π(n:ℕ). Vect n Char -- translated into ∏-notation
replicate 0     = []
replicate (n+1) = '!' :: (replicate n)
~~~~

Most dependently-typed languages offer syntactic sugar to allow
including terms inside the function-arrow syntax. In fact, we actually
can see that for some type `P` which does not depend on `x`, `Π(x:T).P`
is exactly equivalent to the plain function type `T → P`. So how is this
really a product?

> **Though this be madness, yet there is method in't!** Let's see what
> happens when we restrict our $\prod$-operator to have the second
> element not depend on the first.

Let's look at a numerical example:

$$\begin{align}
    f_x &= C\\
    \prod_{i=0}^n f_i &= f_0 \times f_1 \times f_2 \times \cdots \times f_n\\
                      &= C^n
\end{align}$$

If `f` is constant over its input, then the multiplication can be
collapsed into exponential notation. Now, the above example can be
understood with an elementary understanding of grade-school numerical
algebra; we can generalize it to hold for type algebra. But remember
that an exponential type $\beta^\alpha$ is just a function type
$\alpha\to\beta$! One way to think of it is to say that it maps every
$\alpha$ into $\beta$-space.

Another way to think of it is to say that the codomain of a function is
modelled by product of all the outputs of that function corresponding to
the domain. For instance, consider the set `B` and some function `name`
that maps bovines to strings:

$$\begin{align}
\textbf{B} &= \{ \text{Bessie}, \text{Brownie}, \text{Macy} \}\\
\textbf{name} &\in \prod_{b\in\textbf{B}}.\ \textbf{String}\\
              &\approx \textbf{String}^\textbf{B}\\
              &\approx \textbf{B} \to \textbf{String}
\end{align}$$<!--___-->

If we'd wanted to specify a function that mapped each bovine to its name
paired with *that specific* bovine, we'd need to use a dependent type.

And so a $\prod$-type is the (cartesian) product of all elements of its
second component corresponding to its first component. In the case that
the former is not dependent on the latter, this is a (cartesian) power:
this is the space that functions of the second-order lambda calculus
inhabit.

It's much the same with $\sum$-types. Let's first take a numerical
example:

$$\begin{align}
    f_x &= C\\
    \sum_{i=0}^n f_i &= f_0 + f_1 + f_2 + \cdots + f_n\\
                     &= C \times n
\end{align}$$

And so the sum of all the outputs, when the output set doesn't depend on
the input value, is just a product. Let's look again at our bovine
type to see how we can get pairs from this:

$$\begin{align}
\textbf{name} &\in \sum_{b\in\textbf{B}}.\ \textbf{String}\\
              &\approx \textbf{String} \times \textbf{B}\\
\end{align}$$<!--__-->

So, in this special non-dependent case, a $\sum$-type is just a
cartesian product (of bovines and strings): it specifies all possible
pairs of bovines and string.

## Realizing $\prod$ and $\sum$ in Agda

I find that nothing helps me understand a concept better than a concrete
example in a language I understand. I'll be using
[Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php) today, but the
same task could have been done in most other dependent-typed languages,
including [Idris](http://idris-lang.org).

~~~~{.Haskell}
module SigmaPi where

-- We use (dependent) records rather than plain-old data declarations
-- because these conveniently provide accessors; an accessor for a
-- Π-type corresponds to function application.
record Π (α : Set) (β : α → Set) : Set where
  constructor Λ
  field _$_ : ((x : α) → β x)

record Σ (α : Set) (β : α → Set) : Set where
  constructor _,_
  field
    fst : α
    snd : β fst

-- A special case of Π: the function arrow.
_~>_ : Set → Set → Set
_~>_ α β = Π α (λ _ → β)

-- A special case of Σ: the cartesian product.
_×_ : Set → Set → Set
_×_ α β = Σ α (λ _ → β)
~~~~

Now, we can test them to see how they work:

~~~~{.Haskell}
module Tests where
  open import Data.Char
  open import Relation.Binary.Core

  open Π
  open Σ

  data Bovine : Set where
    Bessie  : Bovine
    Brownie : Bovine
    Macy    : Bovine

  pair : ℕ × Bovine
  pair = 43 , Macy

  suc' : ℕ ~> ℕ
  suc' = Λ suc

  -- We can prove trivial equalities to test:
  Σ-fst-test : fst pair ≡ 43
  Σ-fst-test = refl

  Σ-snd-test : snd pair ≡ Macy
  Σ-snd-test = refl

  Π-application-test : suc' $ 2 ≡ 3
  Π-application-test = refl
~~~~


## Coming up next: Universe Polymorphism

You may be wondering why I haven't shown how to encode Haskell-style
existentials; the problem is that they are actually impossible under the
implementation I have given above. In the above implementation, the
first part of the $\sum$ is a term of a type, but in order to forget
(for instance) the type of a list's elements, we'd need that first part
to be a term of a kind. This calls for *universe polymorphism*, where
the same code can be parameterized over universe levels; this will allow
us to easily introduce Haskell-style existential types.
