#lang racket
(require (lib "eopl.ss" "eopl"))
(define-datatype thunk thunk?
  (a-thunk
   (expression expr?)
   )
  )

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (list-val
   (lst list?))
  (proc-val
   (proc proc?))
  (non-val)
  )

(define (expval->val exp1)
  (cases expval exp1
    (list-val (lst) lst)
    (bool-val (bool) bool)
    (num-val (num) num)
    (non-val '())))

(define-datatype proc proc?
  (procedure
   (id symbol?)
   (params list?)
   (body list?))
  )
(define-datatype statement statement?
  (s-statement
   (s simple-statement?))
  (c-statement
   (s compound-statement?))
  )

(define-datatype simple-statement simple-statement?
  (assignment-statement
   (id symbol?)
   (right-hand expr?))
  (return-statement
   (body (lambda (x) (or (expr? x) (null? x)))))
  (global-statement
   (id symbol?))
  (pass-statement)
  (break-statement)
  (continue-statement)
  (print-statement
   (arg list?))
  )

(define-datatype compound-statement compound-statement?
  (function-def-statement
   (id symbol?)
   (params list?)
   (body list?))
  (if-statement
   (condition expr?)
   (body list?)
   (else-body list?))
  (for-statement
   (id symbol?)
   (iterable expr?)
   (body list?))
  )
(define-datatype param param?
  (param-with-default
   (id symbol?)
   (expression expr?)
   )
  )

(define-datatype expr expr?
  (disjunction-exp
   (body disjunct?))
  )

(define-datatype disjunct disjunct?
  (simple-disjunct
   (x conjunct?))
  (compound-disjunct
   (x1 disjunct?)
   (x2 conjunct?))
  )

(define-datatype conjunct conjunct?
  (simple-conjunct
   (x inversion?))
  (compound-conjunct
   (x1 conjunct?)
   (x2 inversion?))
  )

(define-datatype inversion inversion?
  (not-inversion
   (x inversion?))
  (comparison-inversion
   (comp comparison?))
  )

(define-datatype comparison comparison?
  (simple-comp
   (x sum?))
  (compound-comp
   (x1 sum?)
   (x2 list?))
  )

(define-datatype comp-op-sum-pair comp-op-sum-pair?
  (eq-sum
   (x sum?))
  (lt-sum
   (x sum?))
  (gt-sum
   (x sum?))
  )

(define-datatype sum sum?
  (addition-sum
   (left-hand sum?)
   (right-hand term?))
  (subtraction-sum
   (left-hand sum?)
   (right-hand term?))
  (simple-sum
   (x term?))
  )

(define-datatype term term?
  (multiplication-factor
   (left-hand term?)
   (right-hand factor?))
  (division-factor
   (left-hand term?)
   (right-hand factor?))
  (simple-term
   (x factor?))
  )

(define-datatype factor factor?
  (plus-factor
   (x factor?))
  (minus-factor
   (x factor?))
  (simple-factor
   (x power?))
  )

(define-datatype power power?
  (to-power
   (left-hand atom?)
   (right-hand factor?))
  (simple-power
   (x primary?))
  )

(define-datatype primary primary?
  (atom-primary
   (x atom?))
  (expression-primary
   (x primary?)
   (expression expr?))
  (empty-primary
   (x primary?))
  (argument-primary
   (x primary?)
   (arguments list?))
  )
(define-datatype atom atom?
  (id-atom
   (x symbol?))
  (boolean-atom
   (x symbol?))
  (none-atom)
  (number-atom
   (x number?))
  (list-atom
   (x list?))
  )

(define (report-unbound-var) (display "unbound variable"))

(define (empty-environment) (list))

(define (extend-environment var ref env) (cons (list var ref) env))

(define (apply-environment var env)
  (if (null? env)
      (report-unbound-var)
      (if (eqv? var (first (first env)))
          (second (first env))
          (apply-environment var (rest env))
          )
      )
  )
(define the-store (list))

(define (refrence? v) (integer? v))

(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val)))
    next-ref)
  )



(define (setref! ref new-val)
  (set! the-store (list-set the-store ref new-val))
  )
(define (extend-env-with-args params args env)
  (if (null? args)
      (extend-env-only-params params env)
      (extend-env-with-args (rest params) (rest args)
                            (extend-env-with-arg (first params) (first args) env))
      )
  )
(define (extend-env-only-params params env)
  (if (null? params)
      env
      (cases param (first params)
        (param-with-default (id exp1)
                            (extend-env-only-params (rest params) (extend-environment id (newref (a-thunk exp1)) env)))
        )
      )
  )

(define (extend-env-with-arg param1 arg1 env)
  (cases param param1
    (param-with-default (id exp1)
                        (extend-environment id (newref (a-thunk arg1)) env))
    )
  )

(provide (all-defined-out))

