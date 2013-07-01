---
title: Terms Indexed by Canonicity Admit Coercion Across Isomorphism
---

Let $\mathcal{U}$ be an impredicative universe defined as follows with
an empty set, a unit set, a boolean set, non-dependent functions,
products, and some notion of equality which we will elaborate upon later:

$$\begin{align*}
\mathcal{U} &:\equiv \{`\text{Set}, `\mathbf{0}, `\mathbf{1}, `\mathbf{2},
\cdot`\to\cdot, \cdot`\otimes\cdot, \cdot`\cong\cdot \}
\end{align*}$$

$\mathcal{U}$ is embedded in Martin-Löf Type Theory with UIP and given with the following translation:

$$\begin{align*}
⟦`\text{Set}⟧ &\mapsto \color{blue}{\text{Set}}\\
⟦`\mathbf{0}⟧,  ⟦`\mathbf{1}⟧, ⟦`\mathbf{2}⟧ &\mapsto \color{blue}{\bot}, \color{blue}{\top},
\color{blue}{\text{Bool}}\\
⟦A\, `\to\, B⟧, ⟦A\, `\otimes\, B⟧ &\mapsto \color{blue}{\prod_{x : ⟦A⟧} ⟦B⟧}, \color{blue}{\sum_{x : ⟦A⟧} ⟦B⟧}\\
⟦A\, `\cong\, B⟧ &\mapsto \color{blue}{⟦A⟧ \equiv ⟦B⟧}\\
\end{align*}$$

We define $\mathcal{U}$ together with a term language, and in a
particularly egregious abuse of notation, conflate the interpreting
functions $⟦\cdot⟧$ for the two.

<!--more-->

In addition to indexing terms by their type in $\mathcal{U}$, let us in
addition index them by their canonicity, or, more precisely, their
ability to be reduced to a canonical term in the metalanguage. We will
color canonical terms in $\mathbf{black}$, and non-canonical terms in
$\mathbf{\color{red}{red}}$. As above, terms in the metalanguage will be
given in $\mathbf{\color{blue}{blue}}$. The term embeddings are given as
follows:

$$\begin{align}
\frac{\vdash x : `\mathbf{0}}{\vdash `!\, x : A}\tag{Absurd}
\end{align}$$

$$\begin{align}
\frac{}{\vdash `\diamond : `\mathbf{1}} \tag{$\mathbf{1}$-intro}
\end{align}$$

$$\begin{align}
\frac{}{\vdash `1, `2 : `\mathbf{2}} \tag{$\mathbf{2}$-intro}
\end{align}$$

$$\begin{align}
\frac{\vdash b : \mathbf{2} \qquad \vdash t, f : A}{\vdash `\text{if}\ x\ \text{then}\ t\ \text{else}\ f : A} \tag{$\mathbf{2}$-rec}
\end{align}$$

$$\begin{align}
\frac{\vdash f : A \qquad \vdash x : A}{\vdash f\ x :
B}\tag{App}
\end{align}$$

We simply embed functions from the metalanguage here:

$$\begin{align}
\frac{\vdash \color{blue}{e : \prod_{a:A}B}}{\vdash
`\lambda\color{blue}{e} : A\,`\to\,B}\tag{Lam}
\end{align}$$

$$\begin{align}
\frac{\vdash x : A \qquad \vdash y : B}{\vdash x\,`,\,y :
A\,`\otimes\,B}\tag{Pair}
\end{align}$$

$$\begin{align}
\frac{\vdash p : A\,`\otimes\,B}{\vdash `\pi_1\ p : A \qquad \vdash
`\pi_2\ p : B}\tag{Proj}
\end{align}$$


Because the metalanguage we have chosen respects the Unicity of Identity
Proofs, we can have a more flexible embedding of reflexivity in our
object language, leaving the evidence to the metalanguage:

$$\begin{align}
\frac{\vdash x, y : A \qquad \vdash \color{blue}{p : ⟦x⟧ \equiv  ⟦y⟧}}{\vdash `\text{refl}_A\ x\ y\ \color{blue}{p} : x\,`\cong_A\,y}\tag{Refl}
\end{align}$$

And now for the fun part: we use the full force of the metalanguage to
prove a coherent isomorphism between two object language types, and then
we simply assert that it is an identity, with the caveat that it is not
canonical.

$$\begin{align}
\frac{\vdash \color{blue}{f : \prod_{x:⟦A⟧}⟦B⟧} \quad \vdash
\color{blue}{g : \prod_{x:⟦B⟧}⟦A⟧} \quad \vdash \color{blue}{\alpha :
\prod_{x:⟦B⟧} f\ (g\ x)\, \equiv \, x}\quad \vdash \color{blue}{\beta :
\prod_{x:⟦A⟧} g\ (f\ x)\, \equiv \, x}}{\vdash \color{red}{`\text{iso}_{A,B}\ \color{blue}{f\ g\ \alpha\ \beta} : A\,`\cong_{`\text{Set}} B}}\tag{Iso}
\end{align}$$

Note that so far, none of our typing rules have admitted non-canonical
premises, with the result that such a construction cannot escape the
object language! We now provide a rule to eliminate equivalences within
the object language in a safe way, which we will show to be
interpretable into canonical terms of the metalanguage:

$$\begin{align}
\frac{\vdash \color{red}{p : A\,`\cong B} \qquad \vdash x : A}{\vdash\text{transport}\ \color{red}{p}\ x : B} \tag{Transport}
\end{align}$$

Now this is not as powerful as $\text{transport}$ in typical type
theories, in that it doesn't admit mapping over a fibration $P :
A\,`\to\,`\text{Set}$ to get a term in $P\ B$; it may be possible to
make that work, but I am afraid it is beyond my skill-level on this very
day.

By trapping the axiom that isomorphism implies equality between two
rules $\text{Iso}$ and $\text{Transport}$ and indexing our terms by a
notion of canonicity, we have made it possible to ensure that
isomorphisms are translated out of any terms that may be constructed and
passed to the interpreter. Let us define the interpreter, and then do
the typical demonstrations for such a thing:

$$\begin{align*}
⟦`!\,x⟧ &\mapsto \color{blue}{!\,⟦x⟧}\\
⟦`\diamond⟧ &\mapsto \color{blue}{\text{tt}}\\
⟦`1⟧, ⟦`2⟧ &\mapsto \color{blue}{\text{true}}, \color{blue}{\text{false}}\\
⟦`\text{if}\ x\ \text{then}\ t\ \text{else}\ f⟧ &\mapsto \color{blue}{\text{if}\ ⟦x⟧\ \text{then}\ ⟦t⟧\ \text{else}\ ⟦f⟧}\\
⟦f\ x⟧ &\mapsto \color{blue}{⟦f⟧\ x}\\
⟦`\lambda\,\color{blue}{e}⟧ &\mapsto \color{blue}{\lambda\,x. ⟦e\ x⟧}\\
⟦x\,`,\,y⟧ &\mapsto \color{blue}{⟦x⟧\,,\,⟦y⟧}\\
⟦`\pi_1\ p⟧, ⟦`\pi_2\ p⟧ &\mapsto \color{blue}{\pi_1\ ⟦p⟧}, \color{blue}{\pi_2\ ⟦p⟧}\\\
⟦`\text{refl}_A\ x\ y\ \color{blue}{p}⟧ &\mapsto \color{blue}{p}\\
⟦`\text{transport}\ \color{red}{(`\text{iso}_{A,B}\ \color{blue}{f\ g\ \alpha\ \beta})}\ x⟧ &\mapsto \color{blue}{f\ ⟦x⟧}
\end{align*}$$

Now, we can see how these isomorphisms work out in practice. Let's
demonstrate that the identity and swapping functions for pairs are in
fact equivalences:

$$\begin{align*}
\color{blue}{\text{id}} &\mapsto\color{blue}{\lambda x.\
x}\\
\color{blue}{\text{swap}} &\mapsto \color{blue}{\lambda \{x\,,\,y\}.\ y\,,\,x}\\
\color{red}{\text{id-equiv}_{A`\otimes B}} &\mapsto \color{red}{`\text{iso}_{A`\otimes B,A`\otimes B}\ \color{blue}{\text{id}\ \text{id}\ (\lambda\,\_.\ \text{refl})\  (\lambda\,\_.\ \text{refl})}}\\
\color{red}{\text{swap-equiv}_{A`\otimes B}} &\mapsto \color{red}{`\text{iso}_{A`\otimes B,B`\otimes A}\ \color{blue}{\text{swap}\ \text{swap}\ (\lambda\,\_.\ \text{refl})\  (\lambda\,\_.\ \text{refl})}}
\end{align*}$$

Then, we should like to of course prove the correct computational
behavior of $\text{transport}$:

$$\begin{align}
\text{id-equiv-sound} &: (`\text{transport}\
\color{red}{\text{id-equiv}_{`\mathbf{2}`\otimes`\mathbf{1}}}\
(`1\,,\,`\diamond))\, `\cong_{`\mathbf{2}`\otimes`\mathbf{1}} (`1\,,\,`\diamond)\\
&\mapsto `\text{refl}\ \_\ \_\ \color{blue}{\text{refl}}
\end{align}$$

And when we transport over the swapping isomorphism, we get the expected
results to hold definitionally:

$$\begin{align}
\text{swap-equiv-sound} &: (`\text{transport}\
\color{red}{\text{swap-equiv}_{`\mathbf{2}`\otimes`\mathbf{1}}}\
(`1\,,\,`\diamond))\, `\cong_{`\mathbf{2}`\otimes`\mathbf{1}} (`\diamond\,,\,`1)\\
&\mapsto `\text{refl}\ \_\ \_\ \color{blue}{\text{refl}}
\end{align}$$

And of course, in the spirit of recent developments in Type Theory, this
construction is formalized in Agda[^1] on
[GitHub](https://gist.github.com/jonsterling/5898241) should you wish to
fiddle with it.

[^1]: Well, easy-mode Agda that is, with all the improper options turned
on.
