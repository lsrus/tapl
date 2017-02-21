#lang racket
(require "arith.rkt")

(require rackunit)

(define tests
  (test-suite "arith"
 
   (check-equal? (-> "succ pred 0") '(succ 0))
   (check-equal? (-> "pred 0") 0)
   (check-equal? (-> "pred succ succ 0") '(succ 0))
   (check-equal? (-> "if true then succ 0 else succ succ 0") '(succ 0))
   (check-equal? (-> "if false then succ 0 else succ succ 0") '(succ (succ 0)))
   (check-equal? (-> "if if true then false else true then succ 0 else pred 0") 
                 '(if false then (succ 0) else (pred 0)))
   (check-equal? (-> "if (if false then false else (true)) then succ 0 else pred 0") 
                 '(if true then (succ 0) else (pred 0)))
   (check-equal? (-> "pred succ (if true then 0 else succ 0)") '(pred (succ 0)))   
   (check-equal? (=> "succ pred 0") '(succ 0))
   (check-equal? (=> " 0") 0)
   (check-equal? (=> "pred succ succ 0") '(succ 0))
   (check-equal? (=> "if true then succ 0 else succ succ 0") '(succ 0))
   (check-equal? (=> "if false then succ 0 else succ succ 0") '(succ (succ 0)))
   (check-equal? (=> "if if true then false else true then succ 0 else pred 0") 0)
   (check-equal? (=> "if (if false then false else (true)) then succ 0 else pred 0") '(succ 0) )
   (check-equal? (=> "iszero pred succ (if true then 0 else succ 0)")  'true)
   
))

(require rackunit/text-ui)
(run-tests tests)
