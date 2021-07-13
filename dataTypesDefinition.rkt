#lang racket
(require (lib "eopl.ss" "eopl"))
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


(define-datatype proc proc?
  (procedure
   (id identifier?)
   (params list?)
   (body list?))
  )

(define-datatype simple-statement s-statement?
  (assignment-statement
   (id identifier?)
   (right-hand exp?))
  (return-statement
   (body (lambda (x) (or (exp? x) (null? x)))))
  (global-statement
   (id identifier?))
  (pass-statement)
  (break-statement)
  (continue-statement)
  (print-statement
   (arg list?))
  )

(define-datatype compound-statement c-statement?
  (function-def-stement
   (function proc?))
  (if-statement
   (condition exp?)
   (body list?)
   (else-body list?))
  (for-statement
   (id identifier?)
   (iterable exp?)
   (body list?))
  )

(define-datatype exp exp?
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
  (comparision-inversion
   (comp comparision?))
  )

(define-datatype comparision comparision?
  (simple-comp
   (x sum?))
  (compound-comp
   (x1 sum?)
   (x2 list?))
  )

(define-datatype comp-op-sum-par comp-op-sum-par?
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
   (expression exp?))
  (empty-primary
   (x primary?))
  (argument-primary
   (x primary?)
   (arguments list?))
  )
(define-datatype atom atom?
  (id-atom
   (x identifier?))
  (boolean-atom
   (x identifier?))
  (none-atom)
  (number-atom
   (x number?))
  (list-atom
   (x list?))
  )
