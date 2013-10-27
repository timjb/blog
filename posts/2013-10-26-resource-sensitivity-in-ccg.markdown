----
title: Hyperbaton & Resource Sensitivity in Combinatory Categorial Grammar
----

Baldridge's Multi-Modal extension of Steedman's CCG attempts to account
for variation in rule applicability entirely within the lexicon, by
annotating slash types (function arrows) with modalities, which license
different combining rules: $\bullet$ licenses only simple application,
$\diamond$ simple application and harmonic composition/substitution,
$\times$ simple application and permuting composition/substitution, and
$\star$ licenses all rules. I demonstrate that further refinement is
needed in order to have a grammar which suffices for the most basic
hyperbata of Ancient Greek (and Latin).

<!--more-->

### The CCG cannot express $Y_1$ hyperbaton

It is particularly problematic that Steedman's principles for valid CCG
combining rules make it impossible to construct the ubiquitous Greek phrase <span
lang=gk>τῇδ' ἐν ἡμέρᾳ</span>, or in fact, nearly any of the hyperbata
which are so common in that language. It is of course possible if one
ignores directionality of functor categories entirely, but we end up
with an overly permissive grammar which would admit obvious ungrammaticalities
such as <span lang=gk style="color:red;">\*ἡμέρᾳ τῇ</span> and so forth.

Steedman & Baldridge's four composition rules are as follows, for
modalities $\mu$ and $\nu$:

$$\begin{align}
\dfrac{\diamond\leq\mu \quad \diamond\leq\nu}{
  X/_\mu Y \quad Y/_\nu Z \Rightarrow_\mathbf{B^\triangleright}X/_{\mu\lor\nu}Z
}\tag{forward harmonic composition}\\
%
\dfrac{\times\leq\mu \quad \times \leq\nu}
{
  X/_\mu Y \quad Y\backslash_\nu Z \Rightarrow_\mathbf{B^\triangleright_\times} X\backslash_{\mu\lor\nu} Z
}\tag{forward crossed composition}\\
%
\dfrac{\diamond\leq\mu \quad \diamond\leq\nu}
{
  Y\backslash_\mu Z \quad X\backslash_\nu Y \Rightarrow_\mathbf{B^\triangleleft} X\backslash_{\mu\lor\nu} Z
}\tag{backward harmonic composition}\\
%
\dfrac{\times\leq\mu \quad \times\leq\nu}
{
  Y/_\mu Z \quad X\backslash_\nu Y \Rightarrow_\mathbf{B^\triangleleft_\times} X/_{\mu\lor\nu}Z
}\tag{backward crossed composition}
\end{align}$$

Unfortunately, we should like to construct something like the following
for <span lang=gk>τῇδ' ἐν ἡμέρᾳ</span>:

$$
\dfrac{
  \dfrac{
    \dfrac{\gk{τῇδ’}}{D/N}
    \quad
      \dfrac{\gk{ἐν}}{P/D}
  }{P/N}\color{red}{\mathbf{???}}
  \quad
  \dfrac{\gk{ἡμέρᾳ}}{N}
}{P}\triangleright
$$

There is actually no variant of the composition rule which allows this
kind of structure: the rule would have to apply right to left
harmonically on functor terms which take their arguments on the right.
What we are looking for is something along the lines of the following:

$$\begin{align}
Y/Z \quad X/Y &\Rightarrow_\color{red}{\mathbf{???}} X/Z\tag{not provided, but useful}\\
\end{align}$$

Steedman warns strongly against any recommendations for further combinatory
rules, but I fail to see the alternative in the case of $Y_1$
hyperbaton.


### An example of resource sensitivity under composition

Consider the following phrase which exhibits $Y_2$ hyperbaton:

<div class="gloss">
<div class="aligned" language="gk">
ἐν τοιοῖσδε κειμένη κακοῖς<br/>
in such-\*dat\* lying-\*nom\* evils-\*dat\*
</div>
<div class="unaligned">
"being in such a state of wretchedness" (Euripides' *Hecuba*, l. 969)
</div>
</div>

With some degree of effort, we can construct a fine CCG derivation for this:

$$
\dfrac{
  \dfrac{
    \dfrac{
      \dfrac{\gk{ἐν}}{((N_{nom}/N_{nom})/(N_{nom}/N_{nom}))/D_{dat}}
      \quad
      \dfrac{\gk{τοιοῖσδε}}{D_{dat}/N_{dat}}
    }{((N_{nom}/N_{nom})/(N_{nom}/N_{nom}))/N_{dat}}\mathbf{B}^\triangleright
    \quad
    \dfrac{
      \dfrac{\gk{κειμένη}}{N_{nom}/N_{nom}}
    }{\alpha\backslash(\alpha/(N_{nom}/N_{nom}))}\mathbf{T}^\triangleleft
  }{(N_{nom}/N_{nom})/N_{dat}}\mathbf{B}^\triangleleft_\times
  \quad
  \dfrac{\gk{κακοῖς}}{N_{dat}}
}{N_{nom}/N_{nom}}\triangleright
$$

But it would appear that such $Y_2$ hyperbaton in prepositional phrases
is blocked when there has already been a $Y_1$ hyperbaton. For instance,
in the *Trachinian Women* of Sophocles, there are many instances of both $\gk{ἐν}\ Y_1\cdots
Y_2$ and also $Y_1\ \gk{ἐν}\ Y_2$, but there is not a single instance of
$Y_1\ \gk{ἐν}\cdots Y_2$: that is, when there has been a displacement
of $Y_1$, $Y_2$ must remain glued to the head preposition.

So we may tentatively consider the following sentence ungrammatical, or
at least highly awkward: <span lang=gk style="color:red;">\*τοιοῖσδ' ἐν
κειμένη κακοῖς</span>. Unfortunately, though, if we augment the CCG to
be powerful enough to generate <span lang=gk>τοιοῖσδ' ἐν κακοῖς</span>,
it will also generate the other with equal ease, which we should like to
avoid.

For this reason, I venture as a suggestion that the new composition
schemas to support $Y_1$ hyperbaton should ignore the modalities of
their arguments in favor of the most restrictive $\bullet$, which does
not admit further composition. In this fashion, we can prevent the
addition of this combinator from sending the grammar into
overproduction. So one of the added composition schemas would be the
following (given a sufficiently large grain of salt to take with the
notation):

$$\begin{equation}
\dfrac{\diamond\leq\mu \quad \diamond\leq\nu}
{
  Y/_\mu Z \quad X/_\nu Y \Rightarrow_{\mathbf{ᗺ}^\triangleleft} X/_\bullet Z
}\tag{inverted backward harmonic composition}
\end{equation}$$

With this in hand, we can retry our derivation of <span lang=gk>τῃδ' ἐν
ἡμέρᾳ</span>:

$$
\dfrac{
  \dfrac{
    \dfrac{\gk{τῇδ’}}{D/_\star N}
    \quad
      \dfrac{\gk{ἐν}}{P/_\star D}
  }{P/_\bullet N}\color{green}{\mathbf{ᗺ}^\triangleleft}
  \quad
  \dfrac{\gk{ἡμέρᾳ}}{N}
}{P}\triangleright
$$

Whereas it would remain entirely impossible construct something like
<span lang=gk style="color:red;">\*τῇδ' ἐν ἔφη ἡμέρᾳ τάδε</span>.
