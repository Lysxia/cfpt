Counting Functional Pregroup Types
===

Short version:

We consider words on the set `Z` of relative integers.

In particular, `X` is the smallest set such that

1. It contains the one-letter word `0`.

2. If `a` and `b` are two words of `X`,
then `X` also contains `a L(b)` and `R(a) b`.

`L(a)` is obtained by taking the reverse of `a`
and subtracting one to every letter.

`R(b)` is obtained by taking the reverse of `b`
and adding one to every letter.

---

Find the number `X(n)` of words of length `n`.

---

*Conjecture*: `X(n) = sum(i = 0 .. n, C(2 * n - i, n + i))`.

`C(n, k)` is the binomial coefficient.

