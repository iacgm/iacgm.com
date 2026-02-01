---
date: '2026-01-31'
draft: false
title: "Lambdir: The World's First No-Code, O(0)-Memory, Directory-Oriented Programming Language"
summary: "Finally, the directory-oriented combinatory logic based programming language that the Market been yearning for has arrived."
aliases: ["/p/lambdir"]
---

For a multi-trillion dollar industry built on software, the tech sector really does seem to despise code.

Even before today's agentic workflows & constant prognoses of the death of software engineering as a whole (conveniently always six months away, just outside the realm of provability), many billions of dollars were splashed around to any business promising to eliminate, or even just reduce, the need for code at all.

At the same time, the cost of storage has plummeted, and the cost of memory has skyrocketed. Clearly, resources have been sorely misallocated.

For too long, quixotic programming language designers have [ignored economic reality](https://www.youtube.com/watch?v=XZ3w_jec1v8), and have instead let themselves be led astray by all sorts of pointless ideals. But through [rain, snow, heat, or gloom of night](https://en.wikipedia.org/wiki/United_States_Postal_Service_creed), the Market delivers. And today, with me as its prophet, the invisible hand has done exactly that. You see, the Market doesn't *want* code that's clear, concise, maintainable, safe, performant, elegant, or simple. In fact, the Market doesn't *want* code at all.

What the Market *wants* is combinatory logic, without any code at all, interpreted entirely within your directory structure, so that we can finally do away with code, variables, and memory allocation, while still just barely maintaining Turing-completeness. Enter [Lambdir](https://github.com/iacgm/lambdir).

# Variables

> There are only two hard things in programming: cache invalidation and naming things. - Phil Karlton

Lambdir offers a solution to one of these (the other is presumably left as an exercise to the reader).

In Lambdir, there are no variables at all. Looking through some of my recent codebases, I was appalled to see variables defined all over the place. I knew that if I wanted to create a language that resulted in less code, it would have to be one with few, if any variables or constants. [Combinatory Logic](https://en.wikipedia.org/wiki/Combinatory_logic) was the perfect starting point.

Combinatory Logic is often framed in terms of the [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus), but is in fact quite a bit older, and, in my opinion, much cooler.

In formal logic, variables are finicky to reason about. We need to define an environment in which to store them, a scope in which they're valid, be precise about their values and meanings, resolve naming collisions, and then handwave away these troublesome details in every proof we write. So, in 1924  Moses Schönfinkel, in a [delightful paper](https://content.wolfram.com/sites/43/2020/12/Schönfinkel-OnTheBuildingBlocksOfMathematicalLogic.pdf), found a way to bypass variables in formal logic entirely, using _combinators_. The two most famous of these are `K` & `S`, which act on 2 & 3 arguments respectively, and behave as follows (although the lowercase variables given in this definition are dummies for demonstration purposes, not "real" variables):

```haskell
K x y = x
S x y z = x z (y z)
```

Note that in a more mainstream language, `K x y` would be written `K(x, y)`, but here, parentheses would quickly become cumbersome. Also, this application is left-associative, so `K x y` is equivalent to `(K x) y`.

Functional programmers will recognize `K` as `const`, a function which simply ignores its second argument, but `S` is a more mysterious. That `S` is short for [Verschmelzungsfunktion](https://blog.plover.com/math/combinator-s.html), meaning "fusion function", is not particularly enlightening. Schönfinkel justified it by explaining that it could be used to implement, for example, the function \(log_x (1+x)\) (in Haskell, something like `S log (1+)`). This is even less enlightening. A better explanation is that it's a sort of distribution/factoring function, which we will see in action later on.

Since this is getting complex, let's see some examples (here, `~>` means "reduces to", and can be thought of as `=`):

```haskell
-- We can define the identity function in terms of S & K
I = S K K

-- Proof:
-- I x ~> S K K x
--     ~> K x (K x)
--     ~> x

-- We can also define composition: B f g x = f (g x)
B = S (K S) K

-- Proof:
-- B f g x ~> S (K S) K f g x
--         ~> (K S f) (K f) g x
--         ~> S (K f) g x
--         ~> K f x (g x)
--         ~> f (g x)
```

Again, these are dummy variables for the purposes of showing how these new combinators would act on some other, arbitrary combinators. [^Smullyan]

[^Smullyan]: As a side note, because combinators operate on other combinators, we end up in a very bizarre universe, where we can feed any one combinator into any other, and get a third combinator out. Telling whether any two combinators are "equal" (or even what that means), is nontrivial (both to define, and to determine: this is a classic undecidable problem). The same issue arises in the lambda calculus. Raymond Smullyan made a lovely extended analogy with a forest of birds in [To Mock a Mockingbird](https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird). At this point in the article, we have met his Starling, Kestrel, and Bluebird.

In the Lambda Calculus, we would have defined these terms more directly, using variables:

```haskell
I = λx.x
B = λfgx.f(gx)
```

Since the Lambda Calculus is notoriously Turing-complete, and since we can clearly convert combinators to λ-terms, we might want to ask if any λ-term can be expressed in terms of simple combinators, and, delightfully, the answer is yes, [S & K are complete](https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis)! So  the two languages are (nearly) equivalent (and in particular, both are Turing-complete).

There's a beautiful world to explore here, but we're straying a bit too far from Lambdir and a bit too close to those pesky PL fanatics, and the Market's brow is starting to furrow, so let's get back on track: the point here is that programming languages need not have variables to get things done, we can get by with a couple of predefined combinators instead.

# Arithmetic & I/O

Usually, we don't just execute programs and content ourselves with knowing that computation is being done, we execute programs because we want tasks completed and questions answered, and because the program completes those tasks or computes the answers to those questions. Because of this unfortunate fact, people usually expect their languages to have all sorts of fancy bells and whistles, like input, output, and basic arithmetic (the Market does anyway).

The canonical way of representing numbers in the Lambda Calculus is with [Church Numerals](https://en.wikipedia.org/wiki/Church_encoding):

```haskell
0 = λfx.x
1 = λfx.f(x)
2 = λfx.f(f(x))
3 = λfx.f(f(f(x)))
...
```

This is a perfect definition, which lets us define some basic arithmetic quite easily[^Pred]:

```haskell
-- Some arithmetic operations can then be defined as follows:
successor = λafx.f(afx)
isZero    = λatf.a(λv.f)t  -- t & f represent true & false respectively
(+)       = λabfx.af(bfx)
(*)       = λabfx.a(bf)x
```

[^Pred]: Predecessor and subtraction are decidedly harder to implement. I encourage you to consider how it might be done in terms of the terms given here.

In combinatory logic, these are all much uglier, and it's about the least efficient way arithmetic could be done, anyway, so I bit the bullet and included numbers in Lambdir. But only barely.

Lambdir supports:
1. 32-bit signed integers (and integer literals),
2. `+`: which adds two integers (multiplication & subtraction can be implemented in terms of this)
3. `=`: which reduces to `KI` (or `λfx.x`) if fed 2 equal integers, and `K` (or `λfx.f`) if fed 2 unequal integers
4. Applying arguments to numbers, in the same way we would in the Lambda Calculus. This has the caveat that we'll have several ways of representing the same combinator, which may behave differently: for example, for any `f` & `x`, we have `(K I) f x ~> x` and `0 f x ~> x`, but these terms are not equal (`= (K I) 0` is not well defined). There's no getting around this: determining whether a term is equivalent to a number (let alone determining which number) is undecidable.

I/O presents an additional problem in that in combinatory logic, as in functional languages, there's no clear reduction order. Unlike eagerly evaluated languages where we can simply sequence I/O operations in the order we want them to be executed, in Lambdir it's impossible to know which terms will be evaluated at all and in what order, so we have to be careful with how we introduce I/O operations.

I chose to follow the monadic style used in Haskell, which is an extremely elegant solution to this problem. We introduce two new, specially named[^IOnames] combinators:

[^IOnames]: I usually like to use the labels `?` for inputs & `!` for outputs, but `?` is, heartbreakingly, a reserved directory name on Windows.

```haskell
-- Input combinator
$ f   ~> f n   [where `n` is the next byte read from stdin]
! n f ~> f     [which outputs byte `n` to stdout as a side effect]
```

With this structure in place, reads & writes can be properly ordered.[^redstrat] As an example, here's a simple program to echo a single byte:

```haskell
$ (S ! K)
```

As an exercise, maybe try to write a full `cat` program, which echoes infinitely (`Y` from the next section will be helpful).

[^redstrat]: This only works if we assume a leftmost-first reduction strategy. I use that anyway, since it's easy to implement and it's a normalizing strategy, meaning it never gets stuck in infinite loops while reducing normalizable terms. There are other reduction strategies though, some of which will produce different outputs on, say, `! 0 (! 1 K)`, which contains two reducible terms.

# Bonus features

Lambdir, keeping the programmer ever in mind, also includes some syntactic sugar:

Firstly, the infamous `Y` combinator, which behaves as follows, is builtin:
```haskell
Y f = f (Y f)
```
This is useful for implementing recursion. For (a non-combinatory) example:
```haskell
-- Recursive function:
fib n = if (n <= 1) then n else fib (n-1) + fib (n-2)

-- Y-ified
fib = Y (λfn. if (n <= 1) then n else f (n-1) + f (n-2))
```
This is not easy to grapple with when you first encounter it, but it's worth understanding.

Secondly, Lambdir has support for tuples/arrays, with the following behavior:
```haskell
-- T is a builtin primitive, which takes in a length n, followed by n elements.
-- It returns a function which gets fed those elements as arguments:
T 0       f = f
T 1 a     f = f a
T 2 a b   f = f a b
T 3 a b c f = f a b c
...
```

# O(0)-Memory

The beauty of combinatory logic comes from its computational simplicity. We don't have the tapes, alphabets, and states of Turing machines, we don't have the α-/β-/η-equivalences of the Lambda Calculus, and we definitely don't have the random access memory of modern hardware: just good ol' fashioned term rewriting, as God intended. 

However, after writing a combinatory logic interpreter on my laptop and looking beneath the hood, I found disgusting, blasphemous memory accesses (and even worse, allocations!) in every nook and cranny. Fortunately, we can get around this. In fact, we can not only store, but also execute entire programs of arbitrary complexity _without making any memory allocations at all_.

The idea here is that a Lambdir program is just a tree, with leaves that are either `S`, `K`, `$`, `!`, or a number (represented as `N#`), and so can be transformed into a directory structure without any files at all. For example, `S + (K 2) 2` becomes:

```
├───0
│   └───N2
├───1
│   ├───0
│   │   └───N2
│   └───1
│       └───K
├───2
│   └───+
└───3
    └───S
```

A few things to note:
- We use numbers as directory names to be able to order combinator arguments.
- Arguments are applied "in reverse" (i.e., the first argument has the highest index). This simplifies execution, since we can pop reducible expressions off the top of the stack, leaving later arguments' indices unchanged.
- These are all directories, and there are no files inside any of them at all. This means that this program's directory as a whole consumes a whopping _zero bytes_ of storage.
- We can perform term reduction _in place_, meaning we can reduce terms locally by just moving directories around, without ever having to read in the whole program at once. Since this directory stores zero bytes, that means any Lambdir program uses zero bytes of memory[^caveat], and zero bytes of storage (which windows will happily confirm)! I expect my Turing award to arrive any day now...

![Windows Explorer](/zerobytes.png#center)

[^caveat]: Of course, the interpreter itself consumes a tiny amount of memory reading in filenames and maintaining a call stack of its own. However, if people can get away with claiming hash maps have O(1) access time, then I figure I can get away with saying Lambdir programs consume O(0) memory.

# Programming in Lambdir

Unfortunately, given Lambdir's young age and lack of commercial backing (so far!), there aren't many editors focused on editing your file structure in this way, so I included [a macro](https://github.com/iacgm/lambdir/blob/master/src/cl_macro.rs) which can be used to compile combinatory logic terms into executable directories ([example here](https://github.com/iacgm/lambdir/blob/master/src/bin/gen_dir.rs)). 

Armed with this, let's see what a Fibonacci generator looks like in Lambdir. Given that Lambdir has a very functional flavor, and that Fibonacci generators are the archetypal recursively defined function, it seems natural to use the usual `F n = F (n-1) + F (n-2)` style definition, but an iterative approach is actually simpler to implement. I encourage you to attempt a (more) recursive version on your own.

```haskell
I = S K K      -- I x = x
B = S (K S) K  -- B f g x = f (g x)
V = B (S I) K  -- V x f = f x 

-- [a, b] -> [b, a+b]
step = S                           -- "Distributes" [a,b] over the arguments
        (B (T 2) (V (K I)))        -- [a, b] -> T 2 b
        (S (B + (V K)) (V (K I)))  -- [a, b] -> a + b

-- n -> fib n
fib = S
    (V step)       -- n -> n step
    (K (T 2 0 1))  -- n -> [0,1]
```

Note that these hideous variables will be expanded out at compile-time. Since we have no (explicit) recursion, this always terminates. This is essentially equivalent to the for-loop below:

```python
[a, b] = [0, 1]
for i in 0..n:
    [a, b] = [b, a + b]
return b
```

Normalizing (aka, evaluating) `fib 10` on my machine takes a couple tens of thousands of reduction steps[^counting], and 1100 seconds. Compared to normalizing the same term in RAM, that's a slowdown of 11 million percent[^fails]. [Not bad](https://youtu.be/5TFDG-y-EHs?t=1108). It's also worth noting that it seems creating and destroying so many directories this quickly seems

[^counting]: The exact number depends on what exactly counts as a reduction, for example, should `(K x) y ~> K x y` be counted?
[^fails]: This example only works in release mode, since without tail call recursion optimization, we very quickly overflow the stack.

I would link some Lambdir programs on Github, but Git doesn't support tracking empty directories, and I couldn't bring myself to sully such a beautiful programming language with, say `.keep` dummy files.

# Similar projects

When I get an idea like this one, I don't like to do any research beforehand, since someone's inevitably spoiled the fun. But once it's completed, it's great to explore what other people came up with, and see what novel additions, if any, I've contributed.

Looks like there are a few directory-oriented languages out-there:
- [Folders](https://esolangs.org/wiki/Folders), which leans even more extremely than into the directory shtick in that language names are not meaningful, however the alphabetical order of subdirectories is.
- [FolderCode](https://esolangs.org/wiki/FolderCode), which reads its code from directory names as well, but includes even more information in directory names than I do.
- [Dirst](https://esolangs.org/wiki/Dirst), which seems like a more fleshed out version of FolderCode, though I'm not sure which came first.

It seems like my language is the only one which not only reads code from the directory-structure, but also evaluates programs in it as well. It's also worth noting that, by default, both Unix & Windows have relatively low (default) path length limits (108 characters on Unix, 128 on Windows), which I had in mind when I came up with my encoding: each directory name has length 1, apart from numeric values. It doesn't seem like these languages were designed with that in mind ([Unary Filesystem especially](https://esolangs.org/wiki/Unary_Filesystem)), although Folders can probably store programs using shallower depths than Lambdir. Pretty neat.
