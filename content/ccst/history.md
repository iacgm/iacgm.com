---
date: '2026-02-07'
draft: true
title: "The Foundational Crisis of Mathematics"
summary: "A short history of the origins of Computer Science"
---

> I can remember Bertrand Russell telling me of a horrible dream. He was in the top floor of the University Library, about A.D. 2100. A library assistant was going round the shelves carrying an enormous bucket, taking down books, glancing at them, restoring them to the shelves or dumping them into the bucket. At last he came to three large volumes which Russell could recognize as the last surviving copy of _Principia Mathematica_. He took down one of the volumes, turned over a few pages, seemed puzzled for a moment by the curious symbolism, closed the volume, balanced it in his hand and hesitated...
> 
> \- G. H. Hardy, _A Mathematician's Apology_

People usually think of it as a branch of engineering, but computer science is much, much older than computers. The field was born out of logic, specifically the [foundational crisis of mathematics](https://en.wikipedia.org/wiki/Foundations_of_mathematics#Foundational_crisis) at the start of the last century, when it became clear that mathematicians had a growing pile of troublesome questions, which, though contrived, were increasingly difficult to wave away. 

## Diagonalization

The first of these was Cantor's diagonal argument, which shows that there are more real numbers than natural numbers.[^cantor] If this were not the case, then we could label each real number with a natural number, then construct a new natural number by changing each digit along the diagonal. For example, using binary digits for simplicity:

```
 s1 = .>0  1  0  0  1  0 ...
 s2 = . 0 >1  1  0  0  1 ...
 s3 = . 1  0 >0  1  0  1 ...
 s4 = . 1  1  0 >1  1  0 ...
 s5 = . 0  1  0  1 >1  1 ...
 s6 = . 1  0  0  0  1 >0 ...

 ... (flipping bits along the diagonal)

 s* = . 1  0  1  0  0  1 ...
```

By its construction, this new number `s*` does not appear anywhere in the infinite list above, since it differs from every `sN` by at least one bit. Therefore, the infinite number of reals is "greater" than the infinite number of natural numbers. A generalization of this argument implies an infinite hierarchy of infinities. While this is now a generally accepted oddity, this was [quite controversial](https://en.wikipedia.org/wiki/Controversy_over_Cantor%27s_theory) at the time, and many felt that something must be wrong.

[^cantor]: Actually, Cantor's theorem is the more general fact that for any set \(A\), there is no surjective map \(f : A \rightarrow \mathcal{P}(A)\), meaning \(\mathcal{P}(A)\) has strictly greater cardinality than \(A\).

This is what's called a diagonalization argument, and we will see many, many variations on it, most notably in Gödel's theorem. Using a similar trick, philosopher Bertrand Russell devised his famous paradox:

> Does the set of all sets which do not contain themselves contain itself?

The issue here is that if this set does contain itself, then by definition it shouldn't, and if it doesn't, then it should. This is often restated as the Barber Paradox, given below:

> In a town where the barber shaves all those, and only all those, who do not shave themselves, does the barber shave themselves?

These sorts of paradoxes are sometimes called [_insolubilia_](https://en.wikipedia.org/wiki/Insolubilia), and were studied in the middle ages. Often, we can also go in the _other_ direction, and instead formalize insolubilia, so that something that at first seems like a linguistic curiosity can be turned into a formal, mathematical statement. For example, this will be the case for:

- The Liar Paradox: ("_This sentence is false_")
- The Berry Paradox: ("_The smallest positive integer not definable in under sixty letters_", itself a definition of less than sixty letters)
- Curry's Paradox: ("_If this sentence is true, then pigs fly_")

Note that the key ingredient to each of these paradoxes is self-reference. Each of these statements, directly or indirectly, speaks about _itself_, which in turn allows them to contradict themselves. (If this is reminiscent of Cantor's diagonal argument, it is not by coincidence.)

Also note that self-reference is _not_ inherently paradoxical or even problematic. For example, "this sentence appears in an article about logic" is perfectly sensible (even true). In fact, the sentence "this sentence is _true_" is so non-paradoxical that it can be interpreted as being true _**or**_ false, and would make sense either way. 

## Firm Foundations

This growing list of concerns is reminiscent of similar issues regarding calculus a century prior. What had been invented by Newton and Leibniz as a fascinating but slightly informal field had been made precise by Cauchy's development of analysis. In a similar vein, many mathematicians & philosophers hoped that all of mathematics (beginning with logic), could be given rigor, which would in turn do away with these paradoxical results (or at least, which would help make sense of them).

Most famously, David Hilbert [sought](https://en.wikipedia.org/wiki/Hilbert%27s_program) out a foundation for mathematics which was (among other things):

1. Complete (capable of proving all true statements)
2. Consistent (incapable of proving contradictions)
3. Decidable (allows the truth of a statement to be determined by an algorithm)

The closest thing to such a system at the time was [_Principia Mathematica_ (or _PM_)](https://en.wikipedia.org/wiki/Principia_Mathematica), created by Alfred Whitehead & Bertrand Russell. In order to prevent self-reference (and in particular, Russell's paradox), it included a hierarchy of "types", where objects of each type could only refer to objects of lower type. It was an extremely detailed, rigorous axiomatization of logic & mathematics, which famously took 379 pages to prove `1 + 1 = 2`, noting "The above proposition is occasionally useful." PM is extremely meticulous, and it's hard to think of a work produced with more precision, thought, and care.

None of it mattered, of course.

In 1931, Gödel showed that any system[^Peano] like _PM_ must either contradict itself or fail to prove certain true statements, showing that truth is impossible to capture formally. A flurry of impossibility results soon followed, also by diagonalization. To name a few:

[^Peano]: In this statement and those following it, I am referring to extensions of Peano Arithmetic, which will be explained later.

1. Von Neumann & Gödel independent showed that no (consistent) system could prove its own consistency.
2. Alfred Tarski showed that truth cannot even be formally defined.
3. J. B. Rosser strengthened Gödel's result to use a stronger form of consistency[^Rosser].
4. Simultaneously, both Alonzo Church & Turing independently showed that truth cannot be determined by any deterministic process at all. In passing, they also discovered fundamental limitations on computation, laid the groundwork of computability & complexity theory.
5. Henry Rice showed that almost any interesting property about programs is undecidable.
6. Löb showed that no system can prove its own correctness (i.e, that a proof of \(P\) implies \(P\)).

[^Rosser]: Gödel's original proof assumed a form of consistency called ω-consistency, which acts across infinite domains: for example, the statements \(\exists x F(x)\) and \(\neg F(0), \neg F(1), \neg F(2), ...\) are consistent, but not ω-consistent. [Rosser's Trick](https://en.wikipedia.org/wiki/Rosser%27s_trick) lets us assume only direct consistency. 

This is probably all very confusing. There are subtle differences between each of these results that you should not expect to grasp on a first read, but they will make more sense in time.

## Conclusions

The point here is that truth is a subtle, fickle, difficult thing, and it was in trying to make meaning out of that mess that computer science was born. The field, at least in its infancy, was developed to answer interesting, fundamental questions, including:

- What is true?
- What is provable?
- What is computable?

A common theme here is that diagonalization turns very weak assumptions lead to very strong restrictions. In other words, we will find ourselves facing brick walls in the form of impossibility results left and right. This is a fact which makes it very hard for computer scientists to make much progress on many fundamental problems[^PNP]. On the other hand, it also [guarantees them work](https://en.wikipedia.org/wiki/Full-employment_theorem) ;).

[^PNP]: The obvious example of this is the P vs. NP problem, but even that doesn't do the woefulness of the situation justice. For example, while diagonalization gives us the [Gap Theorem](https://en.wikipedia.org/wiki/Gap_theorem), we really have no good way of showing that any given problem is not "easy". In particular, that any particular problem takes superlinear time (Linear lower bounds are especially uninteresting, because that's how long it takes to read the input). For more on this dire situation, see Richard Lipton's excellent blog, [_Gödel's Lost Letter_](https://rjlipton.com/2009/02/12/bait-and-switch-why-lower-bounds-are-so-hard/).

At the start of university I attended a dinner with one of my professors, who told me he had a colleague who'd dedicated years to an undecidable problem. "Undecidable does not mean 'impossible'", he told me, "It just means the work is never done."

