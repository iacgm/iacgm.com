---
date: '2026-02-07'
draft: true
title: "Models of Computation"
summary: "-"
---

> Once a logical formalism is established one can expect that a systematic, so-to-say computational, treatment of logic formulas is possible, which would somewhat correspond to the theory of equations in algebra.
>
> - David Hilbert

Once we have the language to formally express mathematical statements, the natural next step is to try to find an algorithm to tell which are true and which are false. This is called the _Entscheidungsproblem_, meaning "decision problem".

Before we get there though, we should make concrete what we mean by "algorithm". Setting implementation details side, in their purest form, algorithms take in some input, and return some output.

To keep things simple, we'll start by considering algorithms which tell us whether their input has some property. We say these sorts of algorithms _decide_ a _language_.
