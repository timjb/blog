---
title: Natural Language Semantics: Montague → Martin-Löf
---

My great joy at finally taking logical semantics for natural language at
Berkeley was a bit dampened by the realization that everyone is still
doing semantics in a type theory from the 1940s. Rather than wallowing
in dismay, I have been inspired to think about manifesting semantics in
a modern Constructive theory within the Martin-Löf tradition;[^Luo]
this is the first in a series of posts in that vein.


[^Luo]: After I'd written this, I came across this awesome little paper,
[Type-theoretical semantics with coercive
subtyping](http://www.cs.rhul.ac.uk/~zhaohui/SALT20.pdf)
that, in addition to making a pretty convincing case for subtyping in
the type theories used to encode formal semantics, also explains quite
nicely some of the elementaries which I may gloss over; if you have no
background in modern Type Theory, I suggest you read over at least
Section 2. Note that my technique differs from that described in the
paper, in that I do not have a separate kind `Prop` from `Set` (which is
the type of small types).


<!--more-->

## Basics: The Sadly Typed Lambda Calculus

NL-semantics in the Montague tradition is typically done within a
simply-typed lambda calculus, combined with some fast-and-loose
subset-predicate stuff. I'll try to avoid the nasty bits and just
show you the context you need in order to understand what we're doing.
Let's draw out the grammar and typing rules:

$$\begin{align}
\text{types} &::= e,\ t,\ \langle \text{type}, \text{type} \rangle
\tag{Grammar}\\
\text{connectives} &::= \forall,\ \exists,\ \land,\ \lor,\ \lnot\\
\text{operators} &::= \lambda,\ \iota
\end{align}$$

> $e$ is the type of entities, and $t$ is the type of propositions;
> $\langle e,t\rangle$ is a function from entities to propositions.
> Function types are written thus in angle-brackets as comma-separated
> lists of types, with the comma associating to the right. $\lambda$ is
> the primative abstraction operator, and $\iota$ is the primitive
> singleton-inference operator.

In extensional semantics, $t$ is just a truth value; in an intensional
theory (which would seem to be necessary for reasons including the
existence of hypotheticals and mood in language), $t$ can be thought of
as the set of all worlds in which some proposition holds true.

Typing rules are pretty straight-forward, given a context of
type-assumptions $\Gamma$ (basically a list of values and their types).

$$\begin{align}
\frac{\Gamma, x:\sigma \vdash M:\tau}
     {\Gamma \vdash \lambda x.M : \langle\sigma,\tau\rangle}
\tag{T-abstraction}
\end{align}$$

> For some $x:\sigma$ and $M:\tau$ (where the body of $M$ may contain
> $x$), the lambda abstraction $\lambda x.M$ is a function of type
> $\langle\sigma,\tau\rangle$.

$$\begin{align}
\frac{\Gamma, f : \langle\sigma,\tau\rangle \vdash x : \sigma}
     {\Gamma \vdash f(x) : \tau}
\tag{T-application}
\end{align}$$

> The application of a function $f:\langle\sigma,\tau\rangle$ on a value
> $x:\sigma$ is $f(x):\tau$.

$$\begin{align}
\frac{\Gamma, x:e \vdash P : \langle e,t\rangle}
     {\Gamma\vdash \iota x.P(x) : e}
\tag{T-inference}
\end{align}$$

> For a predicate $P:\langle e,t\rangle$ which is satisfied by only one
> entity, the inference $\iota x.P(x)$ is that entity.

So under this framework, common nouns, adjectives and intransitive verbs
are of type $\langle e,t\rangle$, whereas individuals (like "Tucker" and
42) are of type $e$. The $\iota$-operator is used to map these
$\langle e,t\rangle$ predicates to the contextually relevant value; if
there are multiple (or zero) values in context that satisfy $P$, the
derivation of $\iota x. P(x)$ will crash. So, the following are logical
representations of words and phrases in this framework:

$$\begin{align}
⟦\text{dog}⟧ &= \lambda x.\ \text{‘}x \text{ is a dog’} &&: \langle e,t\rangle\\
⟦\text{the}⟧ &= \lambda P. \iota x. P(x) &&: \langle\langle e,t\rangle,e\rangle\\
⟦\text{the}⟧(⟦\text{dog}⟧) &= \iota x.\ ⟦\text{dog}⟧(x)&&: e\\
⟦\text{love}⟧ &= \lambda x. \lambda y.\ \text{‘}y \text{ loves } x\text{’} &&: \langle e,e,t\rangle
\end{align}$$

And so forth. In this representation, note that we define primitive
predicates using an existing token in the metalanguage (in this case,
English); it could have Greek or Latin.

## Semantics in a Modern TT: Shifted Up a Level

In modern type theory, we have to sort of rejigger things a bit. The new
world order is that simple nouns (formerly $\langle e,t\rangle$) are
actually just types; so, rather than saying $⟦\text{dog}⟧(x)$, we now
say $x:⟦\text{dog}⟧$. We've actually hoisted most of our machinery into
the type-level; under the Curry-Howard Correspondence, types are
propositions, and terms are proofs of those propositions.

In our intensional interpretation of natural language semantics, then,
different proofs will exist in different worlds; so, in all worlds,
there is a proposition $P$, but it will be provable in only some subset
of worlds. So, to interpret the truth-value of a $P$ in some world, we
are really trying to see if there exists a term of type $P$ in that
world. In a later post, I'll talk about ways that we can express the
concept of multiple worlds in the new framework.

Actual predicates are types indexed by values (so-called
$\prod$-types); values of a predicate type are proofs that the
saturated predicate (proposition) holds. Modification of predicates (as
in "heavy book") is done using $\sum$-types.[^pi-sigma]

[^pi-sigma]: I hate to toot on my own horn, but if you need to read up
on $\prod$ and $\sum$-types, I have written a
[few](http://www.jonmsterling.com/posts/2012-09-07-pi-is-for-power-sigma-for-product.html)
[things](http://www.jonmsterling.com/posts/2012-09-08-adding-universe-polymorphism.html)
about those previously that may be helpful.

To avoid getting mired in words, we can very simply encode this in
[Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php), a
dependently-typed programming language with a few features that will
prove very useful to us.

## Agda: Let's Formalize Our Semantics

Today, we'll only show how to represent what we have currently
demonstrated for the Montague-style theory. For those of you who don't
have much programming experience (let alone experience with Agda), I'll
try to leave helpful comments as we go. Let's get started!

~~~~{.Haskell}
module Semantics where
~~~~

We have a type for cats; in this context, there is only one cat.  We'll
be using `⟦double-brackets⟧` for values which are referring to a word or
phrase.

~~~~{.Haskell}
  data ⟦cat⟧ : Set where
    ⟦Emma⟧ : ⟦cat⟧
~~~~

Our first order of business is to show how we can derive "the".

First, we need to define propositional equality. Agda's standard library
actually has this built-in, but we'll define it for ourselves so that we
can see how it works.

~~~~{.Haskell}
  data _≡_ {A : Set} : A → A → Set where
    refl : {x : A} → x ≡ x
~~~~

The $ι$ operator can be seen as a proposition that for some proposition
$P$, there is one-and-only-one proof (or, in other words, only one term
inhabits type $P$).

A proof that a set $P$ is singleton would consist of the
single proof $x$, and a proof that all proofs of $P$ are equal to
$x$; that's a pair, in which the second item (a function from a
value to a proof that it is unique in some set $P$) is dependent
upon the first item, which is the value which we're trying to
prove is the only inhabitant of $P$. This is a perfect use for a
$\sum$-type! Let's define what that is:

~~~~{.Haskell}
  record ∑ (A : Set) (B : A → Set) : Set1 where
    constructor _,_
    field
      fst : A
      snd : B fst
~~~~

We can define some syntactic sugar to make this a little easier to use:

~~~~{.Haskell}
  syntax ∑ A (λ x → B) = ∑[ x ∶ A ] B
~~~~

And so, a singleton-proof is just a specific instantiation of $\sum$:

~~~~{.Haskell}
  Singleton : Set → Set1
  Singleton P =  ∑[ x ∶ P ] (∀ y → y ≡ x)
~~~~

> "For some proof $x$ of $P$, every proof $y$ of $P$ is equal to $x$." In other
> words, $x$ is the only proof of $P$.

Using the `_,_` constructor from $\sum$, we can make a proof that
`⟦cat⟧` is a singleton set:

~~~~{.Haskell}
  cat-singleton : Singleton ⟦cat⟧
  cat-singleton = ⟦Emma⟧ , λ { ⟦Emma⟧ → refl }
~~~~

We now have enough machinery to define the ι operator.  Because
we want to *infer* the proof that `P` is a singleton, we place
that parameter in double-curly-braces; this is called an
"instance argument" in Agda. If we did not do this, we would have
to provide a proof that `P` is a singleton with every use of ι.

~~~~{.Haskell}
  ι : (P : Set) → {{proof : Singleton P}} → P
  ι _ {{x , _}} = x
~~~~

 <!--_-->

It turns out that `⟦the⟧` is actually the exact same thing as $\iota$!

~~~~{.Haskell}
  ⟦the⟧ : (P : Set) → {{proof : Singleton P}} → P
  ⟦the⟧ = ι
~~~~

And now, to let all our hard work pay off, we can show that ⟦the⟧
really does work as we had hoped!

~~~~{.Haskell}
  ⟦the-cat⟧ : ⟦cat⟧
  ⟦the-cat⟧ = ⟦the⟧ ⟦cat⟧
~~~~

If `⟦cat⟧` had been inhabited by more values, the program would have
failed to typecheck (since it would have been impossible to construct a
proof that `⟦cat⟧` is a singleton).


## Next steps

Some things that still need to be discussed in a future post:

1. How can we unify our new view of semantics with the possible-worlds
   interpretation? How can we assert a "world-signature" (that says what
   kind of types and propositions we have), but allow the specific
   available theorems to differ from world-to-world?

2. How can we coerce a function of a more general type to a more
   specific type? For instance, "eat" might be represented by a type:
   `⟦animal⟧ → ⟦food⟧ → Set`, but it should be possible to treat it as a
   function `⟦cat⟧ → ⟦tuna⟧ → Set`. (For more on this, see Luo's paper
   in the notes.)

