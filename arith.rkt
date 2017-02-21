#lang racket

(require "parse.rkt")
(provide -> ->* =>)

(define (-> prog) 
  (reduce-term (parse prog)))

(define (->* prog) 
  (evaluate-term (parse prog)))
  
(define (=> prog)
  (big-step (parse prog)))

(define (reduce-term term)
  (match term
    ;if expressions
    [`(if true then ,t2 else ,t3) t2]
    [`(if false then ,t2 else ,t3) t3]
    [`(if ,t1 then ,t2 else ,t3)
     `(if ,(reduce-term t1) then ,t2 else ,t3)]
    ;iszero
    [`(iszero 0) 'true]
    [`(iszero ,t1) 
     (if (numeric? t1)
         'false
         `(iszero ,(reduce-term t1)))]
    ;succ
    [`(succ ,t1) `(succ ,(reduce-term t1))]
    ;pred
    ['(pred 0) 0]
    [`(pred (succ ,t1)) (if (numeric? t1) t1 `(pred (succ ,(reduce-term t1))))]
    [`(pred ,t1) `(pred ,(reduce-term t1))]
    [_ (error "Stuck")]))

(define (numeric? term)
  (match term
    [0 #t]
    [`(succ ,t1) (numeric? t1)]
    [_ false]))

(define (value? term)
  (match term
    [`true #t]
    [`false #t]
    [_ (numeric? term)]))

(define (big-step term)
  (if (value? term)
      term
      (match term
        [`(if ,t1 then ,t2 else ,t3)
         (let [(t1-p (big-step t1))]
           (cond [(eq? t1-p 'true) (big-step t2)]
                 [(eq? t1-p 'false) (big-step t3)]
                 [else (error "Stuck")]))]
        [`(pred (succ ,t1))
         (let [(t1-p (big-step t1))]
           (if (numeric? t1-p)
               t1-p
               (error "Stuck")))]
        [`(pred 0) 0]
        [`(succ ,t1)
         (let [(t1-p (big-step t1))]
           (if (numeric? t1-p)
               `(succ ,t1-p)
               (error "Stuck")))]
        [`(iszero ,t1) 
         (let [(t1-p (big-step t1))]
           (cond [(eq? t1-p 0) 'true]
                 [(numeric? t1-p) 'false]
                 [else (error "Stuck")]))])))
