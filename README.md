# Arith implementation in racket

A racket implementation of arith from chapter 3 of TAPL.
Input programs can optionally contain parentheses around terms.

Reduce terms
```
(-> "if (if true then false else true) then succ 0 else pred succ 0")
> (if false then (succ 0) else (pred (succ 0)))
```

Evaluate terms
```
(->* "if (if true then false else true) then succ 0 else pred succ 0")
> 0
```
