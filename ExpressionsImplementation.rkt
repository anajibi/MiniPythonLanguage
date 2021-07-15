#lang racket
(require (lib "eopl.ss" "eopl"))

(define (value-of-expression exp1 env)
  (cases exp exp1
    (disjunction-exp (body)
                     (value-of-disjunction body env))))

(define (value-of-disjunction body env)
  (cases disjunct disjunct1
    (simple-disjunct (x)
                     (value-of-conjunction x env))
    (compound-disjunct (x1 x2)
                       (or (value-of-disjunction x1 env)
                           (value-of-disjunction x1 env)))))

