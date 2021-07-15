#lang racket
(require (lib "eopl.ss" "eopl"))
(require "DataTypesDefinition.rkt")

(define (value-of-expression exp1 env)
  (cases exp exp1
    (disjunction-exp (body)
                     (value-of-disjunction body env))))

(define (value-of-disjunction body env)
  (cases disjunct body
    (simple-disjunct (x)
                     (value-of-conjunction x env))
    (compound-disjunct (x1 x2)
                       (or (value-of-disjunction x1 env)
                           (value-of-disjunction x1 env)))))

(define (value-of-conjunction body env)
  (cases conjunct body
    (simple-conjunct (x)
                     (value-of-inversion x env))
    (compound-conjunct (x1 x2)
                       (and (value-of-conjunction x1 env)
                            (value-of-conjunction x1 env)))))

(define (value-of-inversion body env)
  (cases inverstion body
    (not-inversion (x)
                   (not (value-of-inversion x env)))
    (comparison-inversion (comp)
                           (value-of-comparison comp env))))

(define (value-of-comparison body env)
  (cases comparison body
    (simple-comp (x)
                 (value-of-sum x env))
    (compound-comp (x1 x2) '()))) ;TODO

(define (value-of-comp-op-sum-pairs body env) '()) ;TODO

(define (value-of-comp-op-sum-pair precursor body env)
  (cases comp-op-sum-pair body
    (eq-sum (x) (equal? precursor (value-of-sum x env)))
    (lt-sum (x) (< precursor (value-of-sum x env)))
    (gt-sum (x) (> precursor (value-of-sum x env)))))

(define (value-of-sum body env)
  (cases sum body
    (addition-sum (left-hand right-hand) '()) ;TODO
    (subtraction-sum (left-hand right-hand) '()) ;TODO
    (simple-sum (x) (value-of-term x env))))

(define (value-of-term body env)
  (cases term body
    (multiplication-factor (left-hand right-hand) '()) ;TODO
    (division-factor (left-hand right-hand) '()) ;TODO
    (simple-term (x) (value-of-factor x env))))

(define (value-of-factor body env)
  (cases factor body
    (plus-factor (x) x)
    (minus-factor (x) (- x))
    (simple-factor (x) (value-of-power x env))))

(define (value-of-power body env)
  (cases power body
    (to-power (left-hand right-hand) '()) ;TODO
    (simple-power (x) (value-of-primary x env))))

(define (value-of-primary body env)
  (cases primary body
    (atom-primary (x) (value-of-atom x env))
    (expression-primary (x exp1) '()) ;TODO
    (empty-primary (x) '()) ;TODO
    (argument-primary (x args) '()))) ;TODO
