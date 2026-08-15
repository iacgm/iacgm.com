---
date: '2026-02-07'
draft: true
title: "Logic"
summary: "First-Order Logic Logic & Peano Arithmetic"
---

> When a Mathematical Reasoning can be had it's as great a folly to make use of any other, as to grope for a thing in the dark, when you have a Candle standing by you.
>
> \- John Arbuthnot

If we want to make sense of what's true and what isn't, we need to make precise what we're trying to express, what our assumptions are, and what sort of reasoning we consider trustworthy. Logic gives us the language to do this.

## Propositional Logic

The most basic building blocks of logical statements are propositions (which are either true or false), and connectives (which allow us to express relations between them).

In propositional logic, we have some variables \(a, b, c, ... \) which we make claims about using the connectives \(\land\), \(\lor\), \(\lnot\), & \(\rightarrow\), meaning:

- \(p \land q\) : "\(p\) is true **and** \(q\) is true"
- \(p \lor q\) : "\(p\) is true **or** \(q\) is true (or both)"
- \(\lnot p\) : "\(p\) is **not** true"
- \(p \rightarrow q\) : "**If** \(p\) is true, **then** \(q\) is true"

Note that \(p \rightarrow q\) makes no claim about \(q\) when \(p\) is false. In that case, \(p \rightarrow q\) is considered (vacuously) true. If we want to express that \(q\) is true *only* when \(p\) is true, we write \(p \leftrightarrow q\), which is read "\(p\) if and only if \(q\)", or "\(p\) iff \(q\)".

For example, here's a quick game of two-truths-and-a-lie. Two of the statements below are _tautologies_ (meaning they are true regardless of the values of their variables), while the other is _unsatisfiable_ (meaning it is _false_ regardless of the values its variables)

\[ (a \land b \rightarrow c) \leftrightarrow (a \rightarrow (b \rightarrow c)) \]
\[ (p \rightarrow q) \leftrightarrow (\lnot q \rightarrow \lnot p) \]
\[ (p \rightarrow q) \leftrightarrow (\lnot q \rightarrow \lnot p) \]

## First-Order Logic

Propositional logic (also called zeroth-order logic) is only really interesting if you've got interesting propositions, so we need a way to make claims _about_ things. First-order logic (FOL) gives us the ability to do that.

We'll need a vocabulary to do this, so we specify a **_signature_** of functions, constants, & relations (including equality). For example, with the signature \((+, \times)\), we can construct the statement:

\[ a \times a + b \times b = c \times c \]

Note that this signature does not include definitions of the symbols it introduces, nor does it say what values \(a\), \(b\), and \(c\) can take on. In order to actually evaluate this statement, we need a **_structure_** (also called an **_interpretation_**). 

A structure consists of a **_universe_** of values and an assignment to the symbols in the signature. For example, to give the statement above it's usual meaning, our universe could be the natural numbers, \(\mathbb{N}\), and our assignment could map '\(+\)' to addition and '\(\times\)' to multiplication. However, in a different structure, the very same statement could take on a different meaning entirely.

Since we have universe to range over, FOL also gives us **_quantifiers_**: \(\forall\) & \(\exists\) ("for all" & "there exists", respectively), so that if \(P\) is a statement about some variable \(x\):

- \(\forall x P\) : "\(P\) is true of every \(x\)"
- \(\exists x P\) : "\(P\) is true of some \(x\)"

A statement is called **_valid_** if it is true in _every_ structure, and called **_satisfiable_** if it is true in _some_ structure. For example, consider:

\[ \forall x (x = x) \]
\[ \forall x (1 \times x = x) \]

## Second-Order Logic

Second-order logic takes things one step further, and lets us quantify over **_relations_**. For example, if \(T(a, b)\) means "there is a direct train between \(a\) & \(b\)", then first-order logic has no way of expressing "it is possible to travel from \(a\) to \(b\) by train"[^PNP], but second-order logic does (this is what \(P(a, b)\) represents in the admittedly slightly opaque statement below).

\[
    \exists P [ P(a, b) \land (\forall x \forall y (P(x,y) \leftrightarrow (T(x,y) \lor \exists z (P (x, z) \land T(z, y))))]
\]

## Theories

When we want to study a certain object logically, we usually introduce **_axioms_** and see what **_theory_** they describe. The axioms are what we start with, and the theory is a set of statements containing all the consequences of the axioms.

The axioms are something between assumptions and definitions. We usually have some _intended_ interpretation (some structure we're trying to study), and we try to describe by choosing some axioms.

If the axioms are too weak (vague), then there will be true facts left out of our theory. On the other hand, if we add an axiom which is incorrect, then we will be able to prove things which are _not_ true of the intended interpretation (although they may be true of some other, unintended interpretation).

When a theory \(T\) contains a statement \(\phi\), we write \(T \vdash \phi\).

Since this may be starting to get confusing, let's see an example.

## Peano Arithmetic

The most important theory for our purposes will be _Peano Arithmetic_ (PA). This is a theory based on a very simple set of axioms about the natural numbers. Here, \(S\) is intended to mean "successor", so that \(Sx\) is intended to mean \(x + 1\). The other symbols (\(0\), \(\cdot\), \(+\), \(=\)) have their usual intended interpretations. Make sure you understand what each axiom is meant to say.

1. \(\forall x (0 \neq Sx)\)
2. \(\forall x \forall y (Sx = Sy \rightarrow x = y)\)
3. \(\forall x (x + 0 = x)\)
4. \(\forall x \forall y (x+Sy = S(x+y)) \)
5. \(\forall x (x \cdot 0 = 0)\)
6. \(\forall x \forall y (x \cdot Sy = x \cdot y + x)\)

Lastly, we add the **_induction_**[^induction] axiom-schema[^schema]:

7. For any first-order formula \(P\) in terms of a variable \(n\): 
\[ P(0) \land \forall n (P(n) \rightarrow P(Sn)) \rightarrow \forall n P(n) \]

[^induction]: This may be daunting for anyone unfamiliar with [induction](https://en.wikipedia.org/wiki/Mathematical_induction), but it's worth absorbing. A similar, simpler axiom we could have chosen instead, and which might seem clearer, would be: 
\[\forall n (n = 0 \lor \exists m (x = Sm)) \]
This is very similar to induction, but in fact gives [a slightly different theory](https://en.wikipedia.org/wiki/Robinson_arithmetic). Again, it's important to not let our intended interpretations blind us. See if you can think of an interpretation which satisfies this new axiom, but does not satisfy the induction axiom.
Another possibly simpler alternative (which _is_ equivalent to induction) is the **least-number principle**, which states that if some number has some property, then there is a _least_ number with that property:
\[\exists x [\phi (x)]  \rightarrow \exists x' [\phi (x') \land \forall y (y < x \rightarrow \lnot \phi (y))] \]

[^schema]: It's worth noting that this is an infinite collection of first-order axioms fitting the given pattern, and _**not**_ the same as the second-order statement below. More on this later.
\[\forall P [P(0) \land \forall n (P(n) \rightarrow P(Sn)) \rightarrow \forall n (P(n)) ]\]

It's not at all clear whether we've captured the full essence of the natural numbers, but this seems like a good start. In any case, these axioms are definitely not _too_ strong: anything following from these axioms _must_ be true of the natural numbers, and any complete theory of the natural numbers _must_ include these axioms. We call this property of PA **soundness**.

## Modal Logics

All of the above logics deal with a very coarse notion of truth. They let us express that a claim is true, but not, for example, that it is _might_ be true or that it may not be true now, but it will _eventually_ be true. Modal logics let us deal with these more subtle notions (or _modes_) of truth.

Modal logic was born out of philosophy, in order to study what _must_ be the case (what is _necessary_), and what _may_ be the case (what is _possible_). These two are expressed by the symbols \(\Box\) and \(\Diamond\) respectively, so that, for example:

- "\(\Box \Diamond p\)" \(\equiv\) "it _must_ be the case that \(p\) is _possible_."
- "\(\Diamond \Box p\)" \(\equiv\) "it _may_ be the case that \(p\) is _necessary_."

What exactly should be meant here by "possible" and "necessary" has been the subject of much debate among philosophers, but once we have an intended meaning for one, we can define the other in terms of it:
\[ \Diamond p \equiv \lnot \Box \lnot p \]
\[ \Box p \equiv \lnot \Diamond \lnot p \]

In other words, something is _possible_ if it is not _necessarily_ not the case. Conversely, something is _necessarily_ true if it is not _possible_ for it to be false.

Now, taking different meanings for \(\Box\) or \(\Diamond\) gives rise to new logic systems which let us reason about several different, related concepts (usually, in addition to other new modes).

For example, _temporal logic_ lets us reason over time:
- \(\Box p \equiv\) "\(p\) will be true forever" \( \implies\) \(\Diamond \equiv\) "\(p\) will be true eventually"

Similarly, _epistemic logic_ lets us reason about knowledge:
- \(\Box p \equiv \) "We know \(p\)" \(\implies \Diamond \equiv \) "We can't reject \(p\)"

Some other possible interpretations are:
- \(\Box p \equiv\) "we require \(p\) to hold" \(\implies\) \(\Diamond p \equiv\) "we allow \(p\) to hold"
- \(\Box p \equiv\) "\(p\) is always true" \(\implies\) \(\Diamond p \equiv\) "\(p\) is occasionally true"

Later, we'll take a look at _provability logic_ (more on this later):
- \(\Box p \equiv \) "\(p\) is provable" \( \implies \) \(\Diamond p \equiv \) "\(p\) is consistent"

## Conclusion

This little menagerie of logics is just the tip of the iceberg, but you may already be wondering what the point of these formalisms is. This is a common sentiment, and logic gets a bit of a bad wrap for using a lot of jargon to say very little.

However, each of these will be extremely useful in its own special way in what has been called [_The Unusual Effectiveness of Logic in Computer Science_](https://www.cis.upenn.edu/~val/CIS682/UnusualEffectiveness.pdf).
