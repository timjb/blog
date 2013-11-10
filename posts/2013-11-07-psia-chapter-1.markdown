----
title: A Primer of Infinitesimal Analysis: Some Exercises from Chapter 1
----

As I read Bell's *A Primer of Infinitesimal Analysis*, I thought it would be
useful for me to write down my workings of the exercises as I went. The proofs
are in the language of Intuitionistic Type Theory; according to common
practice, I will sometimes omit arguments from application which can easily be
inferred from the others.

<!--more-->

Bell gives several properties of the order relation $< $ on $\mathbb{R}$, which
we will express as follows:

$$\begin{align}
~\mathsf{transitivity} &: \prod_{a,b,c: \mathbb{R}} a < b \to b < c \to a < c\tag{1}\\
\mathsf{strictness} &: \prod_{a:\mathbb{R}} a < a \to \bot\tag{2}\\
\mathsf{compat}_+ &: \prod_{a,b,c:\mathbb{R}} a + c < b + c\tag{3}\\
\mathsf{compat}_\times &: \prod_{a,b,c:\mathbb{R}} a < b \to 0 < c \to a\times c < b\times c\tag{4}\\
\mathsf{distinction} &: \prod_{a:\mathbb{R}} 0 < a \lor a < 1\tag{5}\\
\mathsf{ordering} &: \prod_{a,b:\mathbb{R}} (a = b \to \bot) \to a < b \lor b < a\tag{6}
\end{align}$$

<div class="theorem">
**Exercise 1.1** Show that (i) $0 < a$ implies $0\neq a$; (ii) $0 < a$ iff $-a < 0$; (iii) $0 < 1 + 1$; and (iv) $(a < 0 \lor 0 < a)$ implies $0 < a^2$.
</div>
<div class="proof">
(i) We employ *reductio ad absurdum* of the proposition that $0 = a$ under the hypothesis $0 < a$ by Bell's strictness axiom.
$$
  (p: 0 < a) \mapsto (q : 0 = a) \mapsto \mathsf{strictness}\ (\mathsf{transport}\ (x\mapsto 0 < x)\ q\ p)
$$
(ii) The equivalence is given on both sides by Bell's axiom that $< $ is compatibible with addition:
$$
\langle \mathsf{compat}_+ \{c:\equiv -a\},\ \mathsf{compat}_+ \{c:\equiv a\} \rangle
$$
(iii) By the definition of the addition operator, this proposition reduces to $0 < 2$, which holds by definition.
(iv) We reason by case disjunction elimination:
$$\begin{align}
\mathsf{left} &:\equiv (p: a < 0) \mapsto \mathbf{let}\ q :\equiv \mathsf{compat}_+\ p\ \mathbf{in}\ \mathsf{compat}_\times\ q\ q\\
\mathsf{right} &:\equiv (p: 0 < a) \mapsto \mathsf{compat}_\times\ p\ p
\end{align}$$
So the full proof is given by $\mathsf{induction}_\lor\ (\_ \mapsto 0 < a^2)\ \mathsf{left}\ \mathsf{right}$.
</div>

<div class="theorem">
**Exercise 1.2** Show that, if $a < b$, then, for any $x$, either $a < x$ or $x < b$.
</div>

<div class="proof">
Classically, we can show that from $a\neq b$, then $\lnot (a = x \land b = x)$, and thence by DM $a\neq x \lor b \neq x$, and thence by case analysis we arrive at the goal. Intuitionistically, the proof is a bit different:
$$\begin{align}
&(p: a < b) \mapsto (x: \mathbb{R})\mapsto \\
&\qquad \mathbf{let}\ (q: 0 < b - a) :\equiv \mathsf{compat}_+\ p\ \mathbf{in}\\
&\qquad \mathbf{let}\ (r : 0 < \frac{x-a}{b-a} \lor \frac{x-a}{b-a} < 1) = \mathsf{distinction}\ \mathbf{in}\\
&\qquad \mathsf{induction}_\lor\ (\_ \mapsto a < x \lor x < b)\\
&\qquad\quad (\mathsf{inl} \circ \mathsf{compat}_+ \circ (\mathsf{compat}_\times\ \_\ q))\\
&\qquad\quad (\mathsf{inr} \circ \mathsf{compat}_+ \circ (\mathsf{compat}_\times\ \_\ q))\\
&\qquad\quad r
\end{align}$$
This is a particularly nice example of how we often immediately reach for the "big guns" of logic and consign ourselves to proof irrelevancy for better or for worse, where there is a perfectly valid approach which allows us to benefit from more the stronger, more general constructive quantifiers.
</div>

<div class="theorem">
**Exercise 1.3** Show that $)a,b($ is empty iff $\lnot (a < b)$.
</div>

<div class="proof">
By the definition of the open interval, the goal for one side of the
equivalence is:
$$
\mathsf{from}\ :\ (a < b \to \bot) \to \sum_{x:\mathbb{R}} a < x \land x < b \to \bot
$$
We can construct a term for this as follows:
$$
\mathsf{from}\ :\equiv\ p \mapsto \langle x, (q, r) \rangle \mapsto p\ (\mathsf{transitivity} \ q\ r)
$$
Therefore, $\lnot (a < b)$ implies that $)a,b( = \emptyset$. The
statement of the other side of the equivalence is as follows:
$$
\mathsf{to}\ :\ \left(\sum_{x:\mathbb{R}}a < x \land x < b \to \bot\right) \to a < b \to \bot
$$
This is given by the following *reductio ad absurdum*:
$$\begin{align}
&p \mapsto (q : a < b) \mapsto\\
&\qquad \mathbf{let}\ (x : \mathbb{R}) :\equiv \frac{b - a}{2}\ \mathbf{in}\\
&\qquad \mathbf{let}\ (s : a < a + x) :\equiv \mathsf{compat}_+\ (\mathsf{compat}_\times\ (\mathsf{compat}_+\ q)\ \mathsf{trivial})\  \mathbf{in}\\
&\qquad \mathbf{let}\ (t : b - x < b) :\equiv \mathsf{trivial}\ \mathbf{in}\\
&\qquad \mathbf{let}\ (u : b = a + (b - a)) :\equiv \mathsf{trivial}\ \mathbf{in}\\
&\qquad \mathbf{let}\ (v : a + x < b) :\equiv \mathsf{transport}\ (z\mapsto z - x < b)\ u\ t\ \mathbf{in}\\
&\qquad p\ \langle a + x,\ (s,\ v)\rangle
\end{align}$$
</div>

