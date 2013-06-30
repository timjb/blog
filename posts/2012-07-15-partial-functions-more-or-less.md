---
title: Partial Functions: more or less than a function?
---

> Funny how we like to talk about total functions. As if there's any
> other kind of function.

This was my somewhat flippant remark this morning, which incited a
conversation about the nature of functions and partial functions,
leading me to learn a few things.

<!--more-->

**But aren't partial functions another kind of function?** This was a
response to my remark, and I think it deserves to be addressed.

The way I see it, a partial function is somewhat *less* than a function,
because it doesn't fulfill all of the requirements of being a function:
namely that it maps every element of its domain $X$ to an element of its
codomain $Y$:

$$\begin{align}
f&: X \mapsto Y\\
p&: X' \mapsto Y\ \text{where}\ X' \subseteq X
\end{align}$$

But another way to look at it, which I hadn't thought of previously, was
that partial functions are a generalization of functions, and so they
are *more* than a function: they broaden the expectations to include
things things that don't conform to the definition of "function" or
"total function".

Does that mean that a partial function is actually *more* than a
function rather than less? No, I don't believe that to be the case.
For instance, rectangles generalize squares to include right-angled
quadrilaterals which are irregular; and so the requirements of *rectangle*
are somewhat less than the requirements of *square*.

We're talking about the size of their requirements, not the number of
conforming entities. In fact, these two things are basically inverse to
each other: as the specificity of requirements increases, the field of
conforming entities narrows.

So in terms of requirements, a partial function is indeed less than a
function. To avoid confusion, the following terminology might help:
*partiality* is a weaker contract than *totality*.
