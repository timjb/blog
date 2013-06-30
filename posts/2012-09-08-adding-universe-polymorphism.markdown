---
title: Pi & Sigma: Adding Universe Polymorphism
---

[Last
time](http://www.jonmsterling.com/posts/2012-09-07-pi-is-for-power-sigma-for-product.html),
I showed how $\prod$ and $\sum$ types generalize the cartesian power
(function arrow) and the cartesian product respectively. Unfortunately,
the Agda implementation I provided requires the first element in the
$\sum$-pair to be a term of a type, which precludes the encoding of
existentials which could be used to abstract away a type (which is a
term of a kind). Today, I'll show how to fix this using Universe
Polymorphism.

<!--more-->

> **What is universe polymorphism?** In our theory, values inhabit
> types, types inhabit kinds, and so forth (it's larger *sorts* all the
> way up!) This stratification wraps up the idea that a proposition
> about a set of terms is their type; and so, if it's possible to have a
> proposition about types, then that means they must have kinds; and if
> we can make propositions about kinds, then they must inhabit terms of
> some larger space, and so forth. It's often useful to make a
> proposition that applies to multiple universes; this is what allows us
> to, for instance, use the same specification for encoding a list of
> values and a list of types.

### Approaches to Universe Levels

The first approach to universe levels is just to ignore them
altogether:

$$\begin{align}
    \tau \in \textbf{Set} \in \textbf{Set} \in \textbf{Set} \in \textbf{Set}\cdots
    \tag{Dragons!}
\end{align}$$

This is unfortunate, and will allow us to prove any proposition, even if
it is false. Agda provides this feature with its `--type-in-type`
option; even though it would "solve" our problem, it is clearly a
nonstarter.

The default approach in Agda is to universe polymorphism, where you
parameterize `Set` with a level index, generating the hierarchy we know
and love:

$$\begin{align}
    \tau \in \textbf{Set}_0 \in \textbf{Set}_1 \in \textbf{Set}_2\cdots
    \tag{Stratification}
\end{align}$$.

The last approach, which is adopted by Idris, is
**Cumulativity**.[^Idris] Cumulativity adds a typing rule that says that
terms in one level are also terms in higher levels:[^McBride]

$$\begin{align}
    \frac{\Gamma \vdash \tau \in \text{Set}_\ell}
         {\Gamma \vdash \tau \in \text{Set}_{\ell+1}} \tag{Cumulativity
Rule}
\end{align}$$

Since we're working in Agda, we will be using the explicit
level-parameterization approach.

[^Idris]: <http://idris-lang.org>
[^McBride]: <http://www.e-pig.org/epilogue/?p=857>


### Adding Universe Polymorphism to $\prod$ and $\sum$

Without further delay, let's rewrite yesterday's code using universe
polymorphism:

~~~~{.Haskell}
module SigmaPi where
  open import Level using (_⊔_)

  -- We specify the universe levels as implicit parameters to the type
  -- constructor. Then, we parameterize instances of `Set` with them.

  -- Our parameters may live in different universes, so the resulting
  -- type needs to live in the smallest universe that can hold both.
  -- The _⊔_ operator selects the maximum level of its arguments.
  record Π {l m} (α : Set l) (β : α → Set m) : Set (l ⊔ m) where
    constructor Λ
    field _$_ : ((x : α) → β x)
  open Π public

  record Σ {l m} (α : Set l) (β : α → Set m) : Set (l ⊔ m) where
    constructor _,_
    field
      fst : α
      snd : β fst
  open Σ public

  -- Not much change is needed to make our special cases work in the
  -- new system.
  _~>_ : ∀ {ℓ} → Set ℓ → Set ℓ → Set ℓ
  _~>_ α β = Π α (λ _ → β)

  _×_ : ∀ {ℓ} → Set ℓ → Set ℓ → Set ℓ
  _×_ α β = Σ α (λ _ → β)
~~~~


Now, we should be able to existentially abstract out both types *and*
values (previously, we could only abstract out values). Let's build up
some tackle:

~~~~{.Haskell}
  -- `exists` "forgets" a parameter of its argument; so, `exists P`
  -- would represent translate to `∃ α. P(α)` in logic.
  exists : ∀ {l m} {α : Set l} → (α → Set m) → Set (l ⊔ m)
  exists = Σ _

  -- We can define some syntactic sugar to get a cleaner, more familiar
  -- abstraction, which allows us to write `∃ α ⇒ P α ℕ` rather than
  -- the more verbose `exists (λ α → P α ℕ)`.
  syntax exists (λ α → P) = ∃ α ⇒ P

  -- Remember, an existential is just a pair containing a value on the
  -- left and a predicate that it satisfies on the right. To simplify
  -- introduction, we can create a little function `∃|` that will infer
  -- the first parameter (such as the length of a vector, or some type
  -- parameter).
  ∃| : ∀ {l m} {α : Set l} {β : α → Set m} {x} → β x → exists β
  ∃| x = _ , x
~~~~

### Hard work pays off: an example use

~~~~{.Haskell}
module Examples where
  open import SigmaPi
  open import Data.Nat

  data Bovine : Set where
    Bessie  : Bovine
    Brownie : Bovine
    Macy    : Bovine

  -- First, let's define a dependent vector type, indexed by its length.
  data Vector {ℓ} (α : Set ℓ) : ℕ → Set ℓ where
    []  : Vector α 0
    _∷_ : {n : ℕ} → α → Vector α n → Vector α (n + 1)
  infixr 5 _∷_

  all-bovines : Vector Bovine 3
  all-bovines = Bessie ∷ Brownie ∷ Macy ∷ []

  -- We can make a vector's length abstract (where the length is a value
  -- rather than a type):
  ∃-value-ex : ∃ n ⇒ Vector Bovine n
  ∃-value-ex = ∃| all-bovines

  -- We can now also make the type of a vector's elements abstract:
  ∃-type-ex : ∃ α ⇒ Vector α 3
  ∃-type-ex = ∃| all-bovines
~~~~

