----
title: Proof-Relevant MCCG: an approach to scrambling in Ancient Greek
----

Within a type-theoretic grammar, we must eventually deal with the issue
of argument-ordering in languages which have (relatively) free word
order. One approach, considered unsatisfactory, is to provide multiple
categorial types for a word, giving the different orders in which it may
accept its arguments; this is embeddable within a minimal CCG calculus.
Another is to make copious use of the type raising operator combined
with composition to construct mountains of unsatisfied clauses: this is
less than optimal, however, given that the type raising operator itself
is problematic for reasons outside the scope of this short exposition.
So today, I shall present two different modest extensions to CCG which
address this issue, namely *something old* and *something new*.

<!--more-->

> For the moment, we shall leave our working grammar of Greek overly
permissive and consider words modulo the direction in which they satisfy
their arguments. As such, rather than the directional slash notation
traditional in CCG, we shall be using bars to indicate unsatisfied
arguments. So, we arrive at a grammar of types given by $N, D, V, P,
\cdot|\cdot$. Finally, I shall work in a restricted subset of CCG which
does not include the type raising operator.


### The problem

When you are satisfying only one argument, its position in relation to the
head is structurally unimportant, given that **MERGE** may occur in
either direction. With two or more arguments, though, it becomes more
complicated. Let us consider the sentence <span lang=gk>ὁ στρατηγὸς ἀπέκτεινε
τοὺς πέρσας</span> ("The general killed the Persians");[^ex1] if we assign the word <span
lang=gk>ἀπέκτεινε</span> the type
$(V\,|D_\textit{nom})|D_\textit{acc}$, then four permutations are
permissable:

[^ex1]: Transliteration: _ho stratēgos apekteine tous persas_

$$
\dfrac{
  \dfrac{\gk{ὁ στρατηγός}}{D_\textit{nom}}
  \quad
  \dfrac{
    \dfrac{\gk{ἀπέκτεινε}}{(V\,|D_\textit{nom})|D_\textit{acc}}
    \quad
    \dfrac{\gk{τοὺς πέρσας}}{D_\textit{acc}}
  }{V\,|D_\textit{nom}}>
  \quad
}{V}<
\tag{1}
$$

$$
\dfrac{
  \dfrac{
    \dfrac{\gk{ἀπέκτεινε}}{(V\,|D_\textit{nom})|D_\textit{acc}}
    \quad
    \dfrac{\gk{τοὺς πέρσας}}{D_\textit{acc}}
  }{V\,|D_\textit{nom}}>
  \quad
  \dfrac{\gk{ὁ στρατηγός}}{D_\textit{nom}}
}{V}>
\tag{2}
$$

$$
\dfrac{
  \dfrac{\gk{ὁ στρατηγός}}{D_\textit{nom}}
  \quad
  \dfrac{
    \dfrac{\gk{τοὺς πέρσας}}{D_\textit{acc}}
    \quad
    \dfrac{\gk{ἀπέκτεινε}}{(V\,|D_\textit{nom})|D_\textit{acc}}
  }{V\,|D_\textit{nom}}<
  \quad
}{V}<
\tag{3}
$$

$$
\dfrac{
  \dfrac{
    \dfrac{\gk{τοὺς πέρσας}}{D_\textit{acc}}
    \quad
    \dfrac{\gk{ἀπέκτεινε}}{(V\,|D_\textit{nom})|D_\textit{acc}}
  }{V\,|D_\textit{nom}}<
  \quad
  \dfrac{\gk{ὁ στρατηγός}}{D_\textit{nom}}
}{V}>
\tag{4}
$$

But any word order which places <span lang=gk>ὁ στρατηγὸς</span> closer
to the verb than <span lang=gk>τοὺς πέρσας</span> cannot be constructed,
because of the order of arguments given by <span
lang=gk>ἀπέκτεινε</span>. And so there, for instance, is no derivation
for the very grammatical sentence, <span lang=gk>ἀπέκτεινε ὁ στρατηγὸς
τοὺς πέρσας</span>.

### Hoffman's Approach: Multiset Combinatory Categorial Grammar (MCCG)

Beryl Hoffman in her 1995 dissertation[^hoffman95] presents a solution
to the above problem by extending the CCG to include multisets for the
arguments of function types. In essence, can now give the type for <span
lang=gk>ἀπέκτεινε</span> as $V\{|D_\textit{acc}, |D_\textit{nom}\}$,
reformulating the typing rules for application from

$$
\dfrac{\vdash e : Y\,|X \quad \vdash e' : X}{\vdash e > e' : Y \quad
\vdash e' < e : Y}
$$

to the following:

$$
\dfrac{\vdash e : Y\{\bar{Z}\} \quad \vdash e' : X \quad \vdash (|X) \in \bar{Z}}{\vdash e > e' : Y\{\bar{Z} - \{|X\}\} \quad
\vdash e' < e : Y\{\bar{Z} - \{|X\}\}}
$$

Now, it is possible to derive the sentence which raw CCG minus type
raising fails with, as follows:

$$
\dfrac{
  \dfrac{
    \dfrac{\gk{ἀπέκτεινε}}{V\{|D_\textit{acc}, |D_\textit{nom}\}}
    \quad
    \dfrac{\gk{ὁ στρατηγὸς}}{D_\textit{nom}}
  }{V\{|D_\textit{acc}\}}>
  \quad
  \dfrac{\gk{τοὺς πέρσας}}{D_\textit{acc}}
}{V}>
$$

[^hoffman95]: Hoffman, Beryl (1995). *The computational Analysis of the Syntax
and Interpretation of "Free" Word Order in Turkish*.


This all seems very nice and good, but in the course of this extension,
we have inadvertently ended up sacrificing one of the most desirable
aspects of the CCG, which is the transparent and deterministic interface
between the semantics and the syntax of a construction. That is,
unlike in plain CCG, from one syntactic derivation, I can construct two
distinct semantic propositions. Let's look at a pathological example
brought about by the double-accusative construction in Greek.

Our sentence will be <span lang=gk>ταῦτα ἔβλαψα ἐκεῖνα</span>,[^ex2] which has
two semantic interpretations: "I harmed these things in those respects"
or "I harmed those things in these respects". If the interpreter from
syntactic to logical form is deterministic (which we should like very
much to be the case), then we should only be able to derive one semantic
interpretation per syntactic derivation. But it turns out, we can't
decide which one to do!

[^ex2]: Translitation: _tauta eblapsa ekeina_

Take the following derivation:

$$
\dfrac{
  \dfrac{
    \dfrac{\gk{ταῦτα}}{D_\textit{acc}}
    \quad
    \dfrac{\gk{ἔβλαψα}}{V\{D_\textit{acc}, D_\textit{acc}\}}
  }{V\{D_\textit{acc}\}}<
  \quad
  \dfrac{\gk{ἐκεῖνα}}{D_\textit{acc}}
}{V}>
$$

There is actually not enough information present in the derivation to
decide which of the arguments in the multiset we substituted first!
Which of those demonstratives is the external object, and which is the
internal object? And since there are no distinguishing features which
the verb can assert on each argument and thence disambiguate them, the
question of which one is which is utterly undecidable in this example. And so
it is clear that we in fact need to reify some notion of substitution
ordering in the proof, since we must compute with it in order to
translate to the unique logical form which corresponds to that
derivation.

> We may draw a parallel here between proof-relevant and
> proof-irrelevant mathematics: the syntactic proofs which are got
> through Ms Hoffman's MCCG would seem to demand a
> proof-relevant approach (that is, one where the particular
> substitution isn't erased and lost to computation), if we are
> interested in doing a translation into logical form by induction on
> the structure of the syntactic proof.

### Proof-Relevant Multiset CCG

It has become clear to me that in order to recover the computational
behavior of syntactic proofs and their translation to logical form, we
must amend the multiset approach to avoid the problem of the
indistinguishability of identical elements. So we must replace multisets
with ordered lists, and then replace the $(|X) \in Z$ membership judgements
with true membership proofs, as follows.

First, we provide ordered lists of types:

$$
\dfrac{}{\vdash \mathsf{List}\ \ \textbf{type}}
\qquad
\dfrac{}{\vdash \mathsf{nil} : \mathsf{List}}
\qquad
\dfrac{\vdash X\ \ \textbf{type} \quad \vdash \bar{X} : \mathsf{List}}{\vdash \{X,\bar{X}\} :
\mathsf{List}}
$$

Now, we give the rules for membership proofs:

$$
\dfrac{\vdash X\ \ \textbf{type} \quad \vdash \bar{X} : \mathsf{List}}{\vdash X \in \bar{X}\ \ \textbf{type}}
\qquad
\dfrac{\vdash X\ \ \textbf{type} \quad \vdash \bar{X} :
\mathsf{List}}{\vdash \mathsf{top} : X \in \{X, \bar{X}\}}
\qquad
\dfrac{\vdash X\ \ \textbf{type} \quad \vdash Y\ \ \textbf{type} \quad
\vdash \bar{X} : \mathsf{List} \quad \vdash p : X \in \bar{X}}{\vdash
\mathsf{pop}\ p : X \in \{Y, \bar{X}\}}
$$

Next, the list subtraction computations:

$$
\dfrac{\vdash X\ \ \textbf{type} \quad \vdash \bar{X} : \mathsf{List}
\quad \vdash p : X \in \bar{X}}{\vdash \bar{X} - p : \mathsf{List}}
$$
$$\begin{align}
\{X, \bar{X}\} - \mathsf{top} &\mapsto \bar{X}\\
\{X, \bar{X}\} - \mathsf{pop}\ p &\mapsto \{X, \bar{X} - p\}
\end{align}
$$

And finally, we adjust the application typing rules (and the rest,
omitted for simplicity of presentation) to include the membership proofs
in their syntax, so that we may use them when computing logical form later:

$$
\dfrac{\vdash e : Y\{\bar{Z}\} \quad \vdash e' : X \quad \vdash p : (|X) \in \bar{Z}}{\vdash e >_p e' : Y\{\bar{Z} - p\} \quad
\vdash e' <_p e : Y\{\bar{Z} - p\}}
$$ <!-- _ -->

With these enhancements, then, the single syntactic derivation we had
for our pathologically ambiguous sentence is recovered as two distinct
syntactic derivations (and our semantic interpreter is once again
deterministic). Thus the following derivations may once again be told
apart:

$$
\dfrac{
  \dfrac{
    \dfrac{\gk{ταῦτα}}{D_\textit{acc}}
    \quad
    \dfrac{\gk{ἔβλαψα}}{V\{D_\textit{acc}, D_\textit{acc}, \mathsf{nil}\}}
  }{V\{D_\textit{acc}, \mathsf{nil}\}}<_\mathsf{top}
  \quad
  \dfrac{\gk{ἐκεῖνα}}{D_\textit{acc}}
}{V}>_\mathsf{top}
$$

$$
\dfrac{
  \dfrac{
    \dfrac{\gk{ταῦτα}}{D_\textit{acc}}
    \quad
    \dfrac{\gk{ἔβλαψα}}{V\{D_\textit{acc}, D_\textit{acc}, \mathsf{nil}\}}
  }{V\{D_\textit{acc}, \mathsf{nil}\}}<_{(\mathsf{pop}\ \mathsf{top})}
  \quad
  \dfrac{\gk{ἐκεῖνα}}{D_\textit{acc}}
}{V}>_\mathsf{top}
$$

and we may proceed by induction on the structure of syntactic
derivations to arrive at a semantic interpretation.

