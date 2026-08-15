---
date: '2026-02-06'
draft: false
title: "A Commuter's Guide to CS Theory"
summary: "-"
aliases: ["/ccst", "/articles/ccst"]
---

## Contents

The topics covered by each article (so far) are listed below:

0. [A Brief History](history)
1. [Logic](logic):
    - Core: 
        - Propositional Logic
        - First Order Logic
        - Circuits
    - Further:
        - Decidable Theories
        - Curry-Howard Isomorphism
2. Computability:
    - Core:
        - Languages
        - Regular Expressions
        - Finite Automata (DFAs)
        - Context Free Grammars (CFGs)
        - Turing Machines
        - Universal Turing Machines
        - Church Turing Thesis
    - Further:
        - Lambda Calculus
        - Church-Turing Thesis
        - Rice's Theorem
3. Complexity:
    - Core:
        - Nondeterminism
    - Further:
        - NP-completeness
4. Gödel's Theorem:
    - Core:
        - Proof outline

The nature of these topics is that they are all very interrelated, with each arguably depending on several others (a [strange loop](https://en.wikipedia.org/wiki/Strange_loop), some might say), so putting them in "order" is nontrivial. Topics are split into "core" sections, which are essentially introductions, and "further" sections, which go deeper and explain the relationship between each topic and the others (and are generally much more fun).

The (vaguely) "recommended" order for total beginners is to go through all the core sections first, and then to revisit the further sections with that context, but a with a little experience can probably hop around at will.

I prioritize presenting ideas in a digestible, concrete, and enlightening way over doing so in a concise or rigorous way, so these articles may contain some roundabout explanations (though hopefully the scenic routes). Those interested in exploring concepts in more detail will benefit from following the links and footnotes sprinkled throughout, but they can also be safely skipped.

## Motivation & Intent

As a teenager who'd been programming for a few years, I'd occasionally stumbled upon CS theory accidentally, usually in the form of StackOverflow answers I didn't understand.

The classic [HTML irregularity incantation](https://stackoverflow.com/a/1732454/8702256) springs to mind, but I'd also been confronted with references to the λ-calculus whilst dabbling into functional programming, the Halting Problem after watching _The Imitation Game_ (unaware that this was not the right "Turing machine"), and a gut-wrenching theorem of Gödel's, which showed the existence of true but unprovable statements (the sort of theorem that itself seems, at first glance, obviously unprovable).

My grasp of each of these topics was tenuous at best, but I was hooked. I picked up a copy of _Gödel, Escher, Bach_, and within a few short chapters, peering down the rabbit hole, one thing became clear to me in the way that it could only be to a seventeen year old: I can't quite make it out, but there's **_something_** down there.

Throughout my senior year, I spent my subway rides, weekends, and evenings inching through articles, textbooks, and papers at a painstakingly slow pace, letting them mostly pass right over my head, but occasionally getting a glimpse at whatever it was that I was chasing. I spent what must have been weeks wading through Turing's _On Computable Numbers_, unable to differentiate between his mistakes and mine. I found a translation of Gödel's _On Formally Undecidable Propositions_ which left me reeling, and with the distinct feeling that I'd met every tree but not the forest. Soon, I'd made up my mind: I would dedicate my college years not to all the mucky stuff computers seem to actually do, but to that mysterious thing they seemed to just hint at.

It seems to me that it's surprisingly difficult to find a computer science degree in the US that gives a strong theoretical foundation. In fact, it seems dangerously easy to complete a CS degree here which covers almost no theory at all. In the US, the term "computer science theory" is so strongly associated with algorithms, data structures, and ML that all the other stuff (i.e., most of the stuff studied in some other places) is often referred to as "eurotheory".[^eurotheory] I don't think a very theoretical degree is the right choice for everyone (or even for most), but I think it's a shame so many CS majors aren't properly introduced to the rabbit hole at all.

[^eurotheory]: Sometimes also called "Volume B" theory. See [here](https://cacm.acm.org/opinion/why-doesnt-acm-have-a-sig-for-theoretical-computer-science/) for a slightly longer discussion. The reasons for this divide are not clear to me, but Dijkstra had some interesting thoughts in his [_On the fact that the Atlantic Ocean has two sides_](https://www.cs.utexas.edu/~EWD/transcriptions/EWD06xx/EWD611.html) having to do with "Buxton index", or how far ahead people and institutions tend to plan. Maybe Europeans, having on average longer Buxton indices than Americans (which he emphasizes is neither good nor bad), tend to care less about the immediate applicability of their work.

I went for a very theory theoretical European degree in the end (One lecturer even started our [Digital Systems course](https://spivey.oriel.ox.ac.uk/corner/Digital_Systems) by announcing "I think you'll find this is the only course in your degree which explains how computers actually work", which was, more-or-less, correct). My whole degree, I was constantly being asked one question: "If you're not actually doing much programming, then what are your classes actually about?" (alongside the much more blunt "So then, what do you actually do all day?").

My goal with this series is to answer that question, and to provide an easy introduction to the core concepts of CS theory, which doesn't demand more time or focus than can be asked of a commuter on a groggy morning or a tired trip home after a long day at work or school. In short, I'm trying to create the guide I would have wanted in high school: something targeting earnest readers with little-to-no background in the field. I'm a firm believer in the 80/20 rule, and I think most interested learners can get the crux of a CS theory education far more efficiently than some might have you believe.

It's fleeting and increasingly rare, but I still sometimes get that same mysterious feeling. It's hard to pin down, but the following quote from the 20th anniversary of _GEB_ comes close:

> I somehow feel a strange inner confidence that the true author of GEB, when one fine day he finally reaches _my_ ripe age, will tender to me the truest of thanks for not having tampered with the vessel into which he poured so much of his young and eager soul - the work that he even went so far as to call, in what some might see as a cryptic or even naively romantic remark, "a statement of my religion". At least _I_ know what he meant.
>
>  \- Douglas Hofstadter
