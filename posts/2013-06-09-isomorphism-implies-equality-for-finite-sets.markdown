---
title: Isomorphism Implies Equality for Finite Sets
---

In a sufficiently simple type theory, it's trivial to demonstrate that
adding an axiom that isomorphism implies equality is safe and
computable. Today we demonstrate this for a universe of finite sets
without induction. Perhaps will help some people start to feel why the
Voevodsky's Univalence Axiom is probably morally correct.

<!--more-->

We follow the [Epigram
Posse](http://sneezy.cs.nott.ac.uk/darcs/RDTP/2ndmeeting/desc.pdf) in
defining enumerations (`EnumU`) by their tags (`UId`) and constructors
(`EnumT`).

~~~~{.haskell}
module Uni where
  postulate String : Set
  {-# BUILTIN STRING String #-}

  data UId : Set where
    ‵_ : String → UId

  infixr 10 _∷_
  data EnumU : Set where
    [] : EnumU
    _∷_ : UId → EnumU → EnumU

  data EnumT : EnumU → Set where
    ‵0   : {t : UId} {E : EnumU} → EnumT (t ∷ E)
    ‵1+_ : {t : UId} {E : EnumU} (n : EnumT E) → EnumT (t ∷ E)
~~~~

Our enumerations subsume the common `Zero` and `One` types:

~~~~{.haskell}
  `zero = []
  ‵one = ‵"tt" ∷ []

  ‵tt : EnumT ‵one
  ‵tt = ‵0
~~~~

Now, we can define our closed type theory.

~~~~{.haskell}
  mutual
    infixl 10 _⊗_
    infixr 10 _≡_
    infixr 10 _≃_

    data Ty : Set where
      set : Ty
      fin : EnumU → Ty
      _⊗_ : Ty → Ty → Ty
~~~~

We provide identity types and a primitive type of isomorphisms. If we
were to have a bit fancier type theory, isomorphisms would be expressed
using a combination of $\Pi$ and $\Sigma$ (rather than be a primitive
construct).

~~~~{.haskell}
      _≡_ : ∀ {T : Ty} (x y : Tm T) → Ty
      _≃_ : ∀ (A B : Ty) → Ty
~~~~

For the sake of simplicity of presentation, our type system is
impredicative. For our purposes, we only care about the first two
universes anyway, so it is not too horrifying. We do a bit of gymnastics
to reflect types into terms; so any term in the metalanguage in `Ty` can
be reflected into a term in the object language of type `set` using
`↓_`.

~~~~{.haskell}
    infixr 10 _,_
    data Tm : Ty → Set where
      ↓_ : Ty → Tm set
      _,_ : ∀ {A B} → Tm A → Tm B → Tm (A ⊗ B)
~~~~

We easily embed terms of our enumeration codes into our type system
using `fin` and our term language using `[_]`.

~~~~{.haskell}
      [_] : ∀ {e} (x : EnumT e) → Tm (fin e)
~~~~

Proofs of identity are introduced by reflexivity, and isomorphisms are
given by two functions going either direction commuting to the identity.

~~~~{.haskell}
      refl : ∀ {A : Ty} {x : Tm A} → Tm (x ≡ x)
      iso : ∀ {A B : Ty}
              (f : Tm (A ⇒ B))
              (g : Tm (B ⇒ A))
              (α : ∀ x y → Tm (((f # (g # y)) ≡ y) ⊗ ((g # (f # x)) ≡ x))) →
              Tm (A ≃ B)
~~~~

Now, we can provide some cool axioms that compute! Trivially, equality
implies isomorphism, but it's also the case that for this universe,
isomorphism implies equality! Now, since we're just writing the typing
rules here, we can say anything we like, but once we define what it
means to be a function, and what it means to apply one, we can show that
these really do compute.

~~~~{.haskell}
      equiv-to-path : ∀ {A B : Ty} → Tm (A ≃ B) → Tm (↓ A ≡ ↓ B)
      path-to-equiv : ∀ {A B : Ty} → Tm (↓ A ≡ ↓ B) → Tm (A ≃ B)
~~~~

Let's go back and define functions for our universe of finite sets.
Since we don't need induction, we can actually just define functions as
finite maps between sets; so, if a set has two terms, a function from
that set to another set is just two terms in the codomain, and so on. We
don't provide any way to eliminate terms of types other than finite sets
and products. And so the codomain is literally on its own, should one
wish to make a function from, say, an identity type. There's probably a
better presentation, but it's not the purpose of this demonstration.

~~~~{.haskell}
    infixr 10 _⇒_
    _⇒_ : Ty → Ty → Ty
    fin [] ⇒ B = fin ‵one
    fin (x ∷ e) ⇒ B = B ⊗ ((fin e) ⇒ B)
    (A ⊗ B) ⇒ C = (A ⇒ B ⇒ C)
    _ ⇒ B = B
~~~~

And now we can define function application, which is basically a
recapitulation of our quirky definition of functions.

~~~~{.haskell}
    infixl 10 _#_
    _#_ : ∀ {A B : Ty} (f : Tm (A ⇒ B)) (x : Tm A) → Tm B
    _#_ {fin []} _ [ () ]
    _#_ {fin (_ ∷ _)} (f , _) [ ‵0 ] = f
    _#_ {fin (_ ∷ _)} (_ , f) [ ‵1+ x ] = f # [ x ]
    _#_ {_ ⊗ _} f (x , y) = f # x # y
    -- We don't provide elimninators for fancy types here.
    _#_ {_ ≡ _} f _ = f
    _#_ {_ ≃ _} f _ = f
    _#_ {set} f _ = f
~~~~

The last bit of chores is to define `subst` for converting across
equalities in the metalanguage (if we had induction for equality types,
we could define it in the object language). In fact, this demonstrates
that both isomorphisms and identities compute! We stop short of proving
that isomorphism and equality are in fact isomorphic (univalence).

~~~~{.haskell}
  subst : ∀ {A B : Ty} (α : Tm (↓ A ≡ ↓ B)) → Tm A → Tm B
  subst {A} {.A} refl x = x
  subst (equiv-to-path (iso f g α)) x = f # x
  subst (equiv-to-path (path-to-equiv p)) x = subst p x
~~~~

Now, we can see the main force our work. Let's define a type of
booleans using our enumeration codes:

~~~~{.haskell}
  ‵bool = ‵"true" ∷ ‵"false" ∷ []
  bool : Ty
  bool = fin ‵bool

  ‵false ‵true : EnumT ‵bool
  ‵true = ‵0
  ‵false = ‵1+ ‵0
~~~~

We can define two simple functions on booleans, the identity and
negation:

~~~~{.haskell}
  id not : Tm (bool ⇒ bool)
  id = [ ‵true ] , [ ‵false ] , [ ‵tt ]
  not = [ ‵false ] , [ ‵true ] , [ ‵tt ]
~~~~

In this universe, there is only one way to make equalities of base
terms; but sets may have many equalities, given all the possibilities
for isomorphism. For instance, the identity is of course an isomorphism,
mapping between sets `{true, false}` and `{true, false}`; we can
construct a proof that the set of booleans is equal to itself:

~~~~{.haskell}
  bool≡bool-id : Tm (↓ bool ≡ ↓ bool)
  bool≡bool-id =
    equiv-to-path
      (iso id id
        (λ{ [ ‵0 ] [ ‵0 ] → refl , refl
          ; [ ‵0 ] [ ‵1+ ‵0 ] → refl , refl
          ; [ ‵0 ] [ ‵1+ (‵1+ ()) ]
          ; [ ‵1+ ‵0 ] [ ‵0 ] → refl , refl
          ; [ ‵1+ ‵0 ] [ ‵1+ ‵0 ] → refl , refl
          ; [ ‵1+ ‵0 ] [ ‵1+ (‵1+ ()) ]
          ; [ ‵1+ (‵1+ ()) ] _ }))
~~~~

In addition, negation is an isomorphism, since it maps between `{true,
false}` and `{false, true}`; so we can give a distinct proof that the
set of booleans is equal to itself:

~~~~{.haskell}
  bool≡bool-not : Tm (↓ bool ≡ ↓ bool)
  bool≡bool-not =
    equiv-to-path
      (iso not not
        (λ{ [ ‵0 ] [ ‵0 ] → refl , refl
          ; [ ‵0 ] [ ‵1+ ‵0 ] → refl , refl
          ; [ ‵0 ] [ ‵1+ (‵1+ ()) ]
          ; [ ‵1+ ‵0 ] [ ‵0 ] → refl , refl
          ; [ ‵1+ ‵0 ] [ ‵1+ ‵0 ] → refl , refl
          ; [ ‵1+ ‵0 ] [ ‵1+ (‵1+ ()) ]
          ; [ ‵1+ (‵1+ ()) ] _ }))
~~~~

And the fun part is that when we `subst` our way across such proofs, our
coercions actually compute, as follows:

~~~~
  subst-boolid : Tm (subst bool≡bool-id [ ‵true ] ≡ [ ‵true ])
  subst-boolid = refl

  subst-boolnot : Tm (subst bool≡bool-not [ ‵true ] ≡ [ ‵false ])
  subst-boolnot = refl
~~~~


