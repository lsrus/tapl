#lang racket

(provide parse)

(define (parse prog)
  (make-term
   (tokens->tree (tokenize prog) expect-rest-empty)
   expect-rest-empty))

(define (tokenize str)
  (regexp-match* #px"\\w+|\\(|\\)" str))

(define (expect-rest-empty lst rest)
  (if (empty? rest)
      lst
      (error "Spare syntax: " rest)))

(define (expect-start lst start)
  (if (and (not (empty? lst)) (equal? (car lst) start))
      (cdr lst)
      (error "Bad syntax, expected" start ': lst)))

(define (prepend elem rest callback)
  (tokens->tree 
   rest
   (lambda (l r)
     (callback (cons elem l) r))))

;;turns list of tokens into tree of symbols based on parentheses
(define (tokens->tree tokens callback)
  (if (empty? tokens)
      (callback '() '())
   (let [(rest (cdr tokens))]
     (match (car tokens)
       ["(" (tokens->tree 
             rest
             (lambda (l r)
               (prepend l (expect-start r ")") callback)))]
       [")" (callback '() tokens)]
       ["0" (prepend 0 rest callback)]
       [token (prepend (string->symbol token) rest callback)]))))

;;helper function for building if terms
(define (build-if lst callback)
  (make-term 
   (expect-start lst 'if)
   (lambda (t1 r1)
     (make-term 
      (expect-start r1 'then)
      (lambda (t2 r2)
        (make-term 
         (expect-start r2 'else)
         (lambda (t3 r3)
           (callback `(if ,t1 then ,t2 else ,t3) r3))))))))
                  
;;turns tree of symbols into full syntax tree
;callback receives:
;-term
;-tree of unused symbols
(define  (make-term list callback)
  (match list
    [`((,term ..1) ,rest ...) 
     (callback (make-term term expect-rest-empty) rest)]
    [`(true ,rest ...) (callback 'true rest)]
    [`(false ,rest ...) (callback 'false rest)]
    [`(0 ,rest ...)
     (callback 0 rest)]
    [`(pred ,rest ..1)
     (make-term rest (lambda (t r) (callback `(pred ,t) r)))]
    [`(succ ,rest ..1)
     (make-term rest (lambda (t r) (callback `(succ ,t) r)))]
    [`(iszero ,rest ..1)
     (make-term rest (lambda (t r) (callback `(iszero ,t) r)))]
    [`(if ,rest ..5)
     (build-if list callback)]
    [other (error "Bad syntax" other)]))