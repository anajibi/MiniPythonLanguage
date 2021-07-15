#lang racket
(require (lib "eopl.ss" "eopl"))
(require "./DataTypesDefinition.rkt")

(define (value-of-expression exp1 env)
  (cases expr exp1
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
  (cases inversion body
    (not-inversion (x)
                   (not (value-of-inversion x env)))
    (comparison-inversion (comp)
                           (value-of-comparison comp env))))

(define (value-of-comparison body env)
  (cases comparison body
    (simple-comp (x)
                 (value-of-sum x env))
    (compound-comp (x1 x2)
                   (value-of-comp-op-sum-pairs (value-of-sum x1) x2))))

(define (value-of-comp-op-sum-pairs precursor body env)
  (cond
    [(null? body) precursor]
    [else (value-of-comp-op-sum-pairs
           (value-of-comp-op-sum-pair precursor (car body) env) (cdr body) env)]))

(define (value-of-comp-op-sum-pair precursor body env)
  (cases comp-op-sum-pair body
    (eq-sum (x) (equal? precursor (value-of-sum x env)))
    (lt-sum (x) (< precursor (value-of-sum x env)))
    (gt-sum (x) (> precursor (value-of-sum x env)))))

(define (value-of-sum body env)
  (cases sum body
    (addition-sum (left-hand right-hand) (add-or (value-of-sum left-hand env)
                                                 (value-of-term right-hand env)))
    (subtraction-sum (left-hand right-hand)
                     (- (expval->val left-hand) (expval->val right-hand)))
    (simple-sum (x) (value-of-term x env))))

(define (add-or left-hand right-hand)
  (cases expval right-hand
    (num-val (num) (+ (expval->val left-hand) num))
    (bool-val (bool) (or bool (expval->val left-hand)))
    (else 0)))

(define (value-of-term body env)
  (cases term body
    (multiplication-factor (left-hand right-hand)
                           (mult-and (value-of-term left-hand env) right-hand)
                           env)
    (division-factor (left-hand right-hand)
                     (/ (expval->val (value-of-term left-hand env))
                        (expval->val (value-of-factor right-hand env))))
    (simple-term (x) (value-of-factor x env))))

(define (mult-and left-hand right-hand env)
  (cases expval left-hand
    (num-val (num) (* num (expval->val (value-of-factor right-hand env))))
    (bool-val (bool) (and bool (expval->val (value-of-factor right-hand env))))
    (else 0)))

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
    (atom-primary (x) (expval->val x))
    (expression-primary (x exp1) '()) ;TODO
    (empty-primary (x) '()) ;TODO
    (argument-primary (x args) '()))) ;TODO
