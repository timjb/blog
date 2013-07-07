---
title: Towards Univalent Observational Type Theory: First Steps
---

Last week I
[proposed](/posts/2013-06-30-terms-indexed-by-canonicity-admit-coercion-across-isomorphism.html)
a way to embed non-canonical equality proofs in a type theory in such a
way as to force that they are eliminated prior to evaluation. This
approach has a few problems. Today, I shall reformulate equality
internally after the manner of Altenkirch, McBride *et al*,[^obseqnow]
and again shy away from the problem of computing congruence or
substitution over functions, which is quite a bit more
difficult.[^laarhoven]

<!--more-->

Let us begin with a very simple inductive-recursive universe
\mathcal{U} (because we are working almost exclusively in the object
language, we shall elide the customary quotation marks which indicate
that these are *codes* for types, not types themselves):

$$\begin{align}
\mathcal{U} &:\equiv \{\mathbf{0}, \mathbf{1}, \mathbf{2},
\prod_{\cdot\,:\,\cdot}\cdot, \sum_{\cdot\,:\,\cdot}\cdot \}
\end{align}$$

And let each code in \mathcal{U} is translated to the equivalent thing
in the metalanguage; we also use $\cdot\to\cdot$ and
$\cdot\times\cdot$ as shorthands for non-dependent products and sums
respectively. We have not included a primitive code of equality in the
universe, because it will actually be *computed* in a type-directed
manner. We use a heterogeneous equality code $x:S\,=\,y:T$ which
specifies the kind of evidence required to say that two terms are
equivalent up to observation.

$$\begin{align}
S:\mathcal{U}\,&=\,T:\mathcal{U} &&\mapsto
\sum_{f:S\to T}\sum_{g:T\to S}((f\circ g):(T\to
T)\,=\,\text{id}_T:(T\to T))\times((g\circ f):(S\to
S)\,=\,\text{id}_S:(S\to S))\\
\_:\mathbf{0}\, &=\,\_:\mathbf{0} &&\mapsto \mathbf{1}\\
\_:\mathbf{1}\, &=\,\_:\mathbf{1} &&\mapsto \mathbf{1}\\
\text{true}:\mathbf{2}\,&=\,\text{true}:\mathbf{2} &&\mapsto
\mathbf{1}\\
\text{false}:\mathbf{2}\,&=\,\text{false}:\mathbf{2} &&\mapsto
\mathbf{1}\\
f:\prod_{x:S}T\, x\,&=\,g:\prod_{y:S'}T'\,y &&\mapsto \prod_{x:S} \prod_{y:S'}
(x:S\,=\,y:S') \to (f\,x):(T\,x)\,=\,(g\,y):(T'\,y)\\
a:\sum_{x:S}T\,x\,&=\,b:\sum_{y:S'}T'\,y &&\mapsto
(\pi_1a:S\,=\,\pi_1b:S') \times (\pi_2a:T\, (\pi_1a)\,=\,\pi_2b:T'\, (\pi_1b))\\
\_:\_\,&=\,\_:\_ &&\mapsto \mathbf{0}
\end{align}$$

By this definition, equality for functions is pointwise and equality
for types is by isomorphism!

Let us note here that in making isomorphism suffice for a proof of
observational equality between types, we must sacrifice proof
irrelevance, something included in the original formulation of
OTT. Since there may be multiple computationally different proofs that
types are isomorphic (such as the isomorphisms of $\mathbf{2}$ with
itself over identity and negation), we cannot truncate proofs in that
way if we hope to have proper coercions which make sense.

As a side effect of our modifications to OTT, it's now quite a bit
easier to implement the coercion function across equal types, since
now it suffices to simply apply the left side of the isomorphism:

$$\begin{align}
s [Q:S\,=\,T\rangle:T \mapsto (\pi_1Q)\,s
\end{align}$$

### Exercises

- **Exercise 1**. Prove that $\lnot\circ(\lnot\circ\lnot):(\mathbf{2}\to\mathbf{2})\,=\,\text{id}_\mathbf{2}:(\mathbf{2}\to\mathbf{2})$.
- **Exercise 2.** Construct two distinct proofs that $\mathbf{2}:\mathcal{U}\,=\,\mathbf{2}:\mathcal{U}$ and demonstrate that coercion $s[Q\rangle$ respects the particular isomorphism $Q$ chosen.
- **Exercise 3.** Construct a proof $Q$ that for all $S,T:\mathcal{U}$, $S\times T:\mathcal{U}\,=\,T\times S:\mathcal{U}$, and demonstrate that for all $x,y$, coercion $(x,y)[Q\rangle\mapsto(y,x)$.
- **Exercise 4.** Extend the type theory we have developed to include computable quotients $S/R:\mathcal{U}$ without introducing axioms to the metalanguage.


[^obseqnow]: Altenkirch, McBride & Swierstra. [Observational Equality,
Now!](http://www.cs.nott.ac.uk/~txa/publ/obseqnow.pdf)

[^laarhoven]: Serendipitously, the formidable Twan van Laarhoven has
been thinking about Univalent OTT lately, and has written
[two](http://twanvl.nl/blog/agda/subst-from-cong)
[pieces](http://twanvl.nl/blog/agda/cong-from-refl) about the
difficulties in getting proper congruence and induction for identity
types in such a theory.

