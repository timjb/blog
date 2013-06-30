---
title: Inferring Typing Derivations for the STLC in Haskell
---

We can define the STLC at the type-level in Haskell, and then provide a type of
derivations (proofs of well-typedness) which is indexed by terms. Further, we
can use Haskell's type classes to infer typing derivations for well-typed terms
in the STLC.

 <!--more-->

First, let us pry open the pandora's box; some of these wights are more
dangerous than others.

> {-# LANGUAGE PolyKinds              #-}
> {-# LANGUAGE DataKinds              #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE GADTs                  #-}
> {-# LANGUAGE FlexibleContexts       #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE KindSignatures         #-}
> {-# LANGUAGE LambdaCase             #-}
> {-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE UndecidableInstances   #-}
> {-# LANGUAGE UnicodeSyntax          #-}

> module STLC where

<h3> The Syntax of the STLC </h3>

First, we will begin with a type-free syntax of terms generating a universe with
a predicative hierarchy of sets:

> data Term
>   = Set Term       -- ^ predicative sets
>   | (~>) Term Term -- ^ the function arrow
>   | ℕ              -- ^ natural numbers
>   | Z              -- ^ zero
>   | S Term         -- ^ successor
>   | Λ Term         -- ^ abstraction
>   | (#) Term Term  -- ^ application
>   | V Term         -- ^ variables

It may seem odd that, for instance, we parameterize `Set` and `V` by `Term`,
since it's clear that we should want natural numbers there. But this will get
nailed down when we make our family of typing rules.

Contexts are either empty or are extended by a type:

> data Cx
>   = E
>   | (:>) Term Cx


<h3> Typing Rules for the STLC </h3>

With the addition of a dirty little bit of syntactic sugar, we can now present
the typing rules of our system.

> type x ⊢ f = f x
> infixr 5 ⊢

I've decided to arrange these rules in a natural-deduction style presentation. A
term of type `γ ⊢ x ∈ τ` says that "the context `γ` proves that `x` is of type
`τ`"; this is called a typing derivation.

> data (∈) :: Term → Term → Cx → ★ where

There is an infinite hierarchy of sets, where each is the type of the previous,
down to the base case, which is the type of small types.

>   TSet  ::           E ⊢ n ∈ ℕ
>           --------------------------------
>          →      γ ⊢ Set n ∈ Set (S n)

`ℕ` is a small type.

>   TNat  :: -------------------------------
>                    γ ⊢ ℕ ∈ Set Z

`Z` is a natural number.

>   TZ    :: -------------------------------
>                       γ ⊢ Z ∈ ℕ

Given a natural number `n`, `S n` is also a natural number.

>   TS    ::            γ ⊢ n ∈ ℕ
>           --------------------------------
>          →           γ ⊢ S n ∈ ℕ

Given a term `l` of type `τ` a context extended by `σ`, `Λ l` is of type `σ ~>
τ`.

>   TLam  ::        σ :> γ ⊢ l ∈ τ
>           --------------------------------
>          →       γ ⊢ Λ l ∈ (σ ~> τ)

Given a term `f` in `σ ~> τ` and a term `x` in `σ`, `f # x` is in `τ`.

>   TApp  ::  γ ⊢ f ∈ (σ ~> τ) → γ ⊢ x ∈ σ
>           --------------------------------
>          →         γ ⊢ f # x ∈ τ

Now we provide evidence that a variable of a given type is within a context.

>   TVTop :: -------------------------------
>                  τ :> γ ⊢ V Z ∈ τ
>   TVPop ::   E ⊢ i ∈ ℕ   →   γ ⊢ V i ∈ σ
>           --------------------------------
>          →     τ :> γ ⊢ V (S i) ∈ σ


<h3> Computing Typing Derivations for Terms </h3>

Using `-XDataKinds` we can construct terms of our lambda calculus using
Haskell's type system as a metalanguage. For instance, $\lambda x.\,
\mathbf{suc}\; x$ is encoded as follows: </h3>

< type PlusOne = Λ (S (V Z))
< plusOne :: E ⊢ PlusOne ∈ (ℕ ~> ℕ)
< plusOne = TLam (TS TVTop)

We of course can also just have our expressions in-line:

< plusOne' :: E ⊢ (Λ S (V Z)) ∈ (ℕ ~> ℕ)
< plusOne' = TLam (TS TVTop)

It would be nice if we could get the process of generating typing derivations
automated for us! Since it's the case that the typing derivation for well-typed
expressions usually resembles the structure of the term very nicely anyway, this
should be possible. We can start with this type class:

> class Infer (x :: Term) (τ :: Term) (γ :: Cx) | x γ → τ where
>   infer :: γ ⊢ x ∈ τ

It turns out that we'll basically just be recapitulating the typing rules
wholesale, in such a way as to get Haskell's constraint solver to infer them for
us:

> instance Infer ℓ ℕ E ⇒ Infer (Set ℓ) (Set (S ℓ)) γ where
>   infer = TSet infer
> instance Infer Z ℕ γ where
>   infer = TZ
> instance Infer n ℕ γ ⇒ Infer (S n) ℕ γ where
>   infer = TS infer

> instance Infer l τ (σ :> γ) ⇒ Infer (Λ l) (σ ~> τ) γ where
>   infer = TLam infer
> instance (Infer f (σ ~> τ) γ, Infer x σ γ) ⇒ Infer (f # x) τ γ where
>   infer = TApp infer infer

> instance Infer (V Z) τ (τ :> γ) where
>   infer = TVTop
> instance (Infer i ℕ E, Infer (V i) σ γ) ⇒ Infer (V (S i)) σ (τ :> γ) where
>   infer = TVPop infer infer

Then, we can make a type for closed expressions that have an inferrable type:

> type Closed e = Infer e τ E ⇒ E ⊢ e ∈ τ

< fst :: Closed (Λ (Λ (V (S Z))))
< fst = infer

< snd :: Closed (Λ (Λ (V Z)))
< snd = infer

<h3>Appendix: Pretty Pretting</h3>

> instance Show (γ ⊢ x ∈ t) where
>   show (TSet ℓ) = "Set" ++ (show . Sub . natToInt $ ℓ)
>   show TNat = "ℕ"
>   show TZ = "Z"
>   show (TS n) = "S (" ++ show n ++ ")"
>   show (TLam l) = "λ. " ++ show l
>   show (TApp f x) = "(" ++ show f ++ ")" ++ " " ++ "(" ++ show x ++ ")"
>   show TVTop = "v" ++ show (varToInt TVTop)
>   show (TVPop _ i) = "v" ++ (show . Sub . varToInt $ i)

> varToInt :: γ ⊢ V i ∈ σ → Int
> varToInt TVTop = 0
> varToInt (TVPop _ i) = 1 + varToInt i

> natToInt :: γ ⊢ n ∈ ℕ → Int
> natToInt TZ = 0
> natToInt (TS i) = 1 + natToInt i
> natToInt _ = error "non-canonical natural number"

> newtype Subscript = Sub Int
> instance Show Subscript where
>   show (Sub n) = fmap trans (show n) where
>     trans =
>       \case
>         '0' → '₀'
>         '1' → '₁'
>         '2' → '₂'
>         '3' → '₃'
>         '4' → '₄'
>         '5' → '₅'
>         '6' → '₆'
>         '7' → '₇'
>         '8' → '₈'
>         '9' → '₉'
>         d   → d

