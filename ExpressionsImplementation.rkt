#lang racket
(require (lib "eopl.ss" "eopl"))
(require "./DataTypesDefinition.rkt")

(define (value-of-program program)
  (begin
    (define env (empty-environment))
    (value-of-statements program env)
    )
 )

(define (value-of-statements statements env)
  (if (null? statements)
      env
      (value-of-statements (rest statements) (value-of-statement (first statements) env))
      )
  )

(define (value-of-statement s env)
  (cases statement s
    (s-statement (s)
                 (value-of-s-statement s env))
    (c-statement (s)
                 (value-of-c-statement s env))
    )
  )

(define (value-of-s-statement s env)
  (cases simple-statement s
    (assignment-statement (id right-hand)
                          (value-of-assignment-statement id right-hand env))
    (return-statement (body)
                      (value-of-return-statement body env))
    (global-statement (id)
                      (value-of-global-statement id env))
    (pass-statement ()
                    env)
    (break-statement ()
                     env)
    (continue-statement ()
                        env)
    (print-statement (lst)
                    (begin
                      (display lst)
                      env)
                    )
    )
  )

(define (value-of-c-statement s env)
  (cases compound-statement s
    (function-def-statement (id params body)
                            (value-of-function-def-statement id params body env))
    (if-statement (condition body else-body)
                  (value-of-if-statement condition body else-body env))
    (for-statement (id iterable body)
                   (begin
                     (value-of-for-statement id iterable body env)
                     env)
                   )
    )
  )

(define (value-of-assignment-statement id right-hand env)
  (let ((new-ref (newref (a-thunk right-hand))))
    (extend-environment id new-ref env)
    )
  )
(define (value-of-return-statement body env)
  (if (null? body)
      (non-val)
      (value-of-expression body env)
      )
  )
(define (value-of-global-statement id env)
  (let ((global-ref (apply-environment id env)))
    (extend-environment id global-ref env)
    )
  )
(define (value-of-function-def-statement id params body env)
  (let ((new-ref (newref (proc-val (procedure id params body)))))
    (extend-environment id new-ref env)
    )
  )
(define (value-of-if-statement condition body else-body env)
  (let ((condition-result (value-of-expression condition env)))
    (if (expval->val condition-result)
        (value-of-statements body env)
        (value-of-statements else-body env)
        )
    )
  )
(define (value-of-for-statement id iterable body env)
  (let ((iterable-value (reverse (expval->val (value-of-expression iterable env)))))
    (begin
      (value-of-for-body id iterable body env)
      env)
    )
  )
(define (value-of-for-body id iterable body env)
  (if (null? iterable)
      env
      (begin
        (let ((result (value-of-for-body id (rest iterable) body env)))
          (cond
            [(eqv? result 'break)
             'break]
            [else
             (value-of-statements-inside-for body (extend-environment id (newref (first iterable)) env))]
            )
          )
        )
      )
  )
(define (value-of-statements-inside-for statements env)
  (if (null? statements)
      env
      (let ((to-execute (first statements)))
        (cases simple-statement to-execute
          (break-statement ()
                           'break)
          (continue-statement ()
                              env)
          (else
           (value-of-statements (rest statements) (value-of-statement to-execute)))
          )
        )
      )
  )

#|(define (value-of-print-statement statement env)
  (cases expval (value-of-statement statement env)
    [num-val (num)
             (display num)
             (newline)]
    [bool-val (bool)
              (display bool)
              (newline)]
    [proc-val (proc)
              (display "<procedure>")
              (newline)]
    [list-val (lst)
              (display lst)
              (newlinw)]
    [non-val
             (display "None")
             (newline)]))|#

#|(define (evaluate path)
  ( (call-with-input-file path (lambda (in) (port->string in)))))|#
  



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
    (addition-sum (left-hand right-hand) (add-or (value-of-sum left-hand env) right-hand env))
    (subtraction-sum (left-hand right-hand)
                     (num-val (- (expval->val left-hand) (expval->val right-hand))))
    (simple-sum (x) (value-of-term x env))))

(define (add-or left-hand right-hand env)
  (cases expval left-hand
    (num-val (num) (num-val (+ (expval->val left-hand) num)))
    (bool-val (bool) (if (eqv? #t bool)
                         (bool-val #t)
                         (value-of-term right-hand env)))
    (else (non-val))))

(define (value-of-term body env)
  (cases term body
    (multiplication-factor (left-hand right-hand)
                           (mult-and (value-of-term left-hand env) right-hand env))
    (division-factor (left-hand right-hand)
                     (num-val (/ (expval->val (value-of-term left-hand env))
                        (expval->val (value-of-factor right-hand env)))))
    (simple-term (x) (value-of-factor x env))))

(define (mult-and left-hand right-hand env)
  (cases expval left-hand
    (num-val (num) (num-val (* num (expval->val (value-of-factor right-hand env)))))
    (bool-val (bool) (if (eqv? #f bool)
                         (bool-val #f)
                          (value-of-factor right-hand env)))
    (else (non-val))))

(define (value-of-factor body env)
  (cases factor body
    (plus-factor (x) (value-of-factor x env))
    (minus-factor (x) (- (num-val (expval->val (value-of-factor x env)))))
    (simple-factor (x) (value-of-power x env))))

(define (value-of-power body env)
  (cases power body
    (to-power (left-hand right-hand) (value-of-to-power left-hand right-hand env))
    (simple-power (x) (value-of-primary x env))))

(define (value-of-to-power left-hand right-hand env)
  (let ((right-hand-val (expval->val (value-of-factor right-hand env)))
        (left-hand-val (expval->val (value-of-atom left-hand env))))
    (num-val (expt left-hand-val right-hand-val))
    )
  )
(define (value-of-atom val env)
  (cases atom val
    (id-atom (x)
             (let ((ref (apply-environment x env)))
               (let ((value (deref ref)))
               (cases thunk value
                 (a-thunk (exp1)
                          (let ((result (value-of-expression exp1 env)))
                            (setref! ref result)
                            result)
                          )
                 (else
                  value)))))
  (boolean-atom (x)
                (bool-val (to-boolean x)))
  (none-atom ()
             (non-val))
  (number-atom (x)
               (num-val x))
  (list-atom (x)
             (list-val x))
    )
  )
(define (to-boolean x)
  (eqv? x 'True))
     
(define (value-of-primary body env)
  (cases primary body
    (atom-primary (x) (value-of-atom x env))
    (expression-primary (x exp1) (value-of-expression-primary x exp1 env))
    (empty-primary (x) (value-of-empty-primary x env))
    (argument-primary (x args) (value-of-argument-primary x args env))))

(define (value-of-expression-primary x exp1 env)
  (value-of-expression (list-ref (expval->val (value-of-primary x)) (expval->val (value-of-expression exp1))))
  )
(define (value-of-empty-primary x env)
  (let ((val (expval->val (value-of-primary x))))
    (cases proc val
      (procedure (id params body)
                 (apply-procedure id params body (list) env)
                 )
      )
    )
  )
(define (apply-procedure id params body args env)
  (value-of-statements body (extend-environment id
                                                (newref (proc-val (procedure id params body)))
                                                (extend-env-with-args params args env)))
  )
  
(define (value-of-argument-primary x args env)
  (let ((val (expval->val (value-of-primary x))))
    (cases proc val
      (procedure (id params body)
                 (apply-procedure id params body args env)
                 )
      )
    )
  )
(define (value-of-thunk t env)
  (cases thunk t
    (a-thunk (exp1)
             (value-of-expression exp1 env)
             )
    )
  )
(define (deref ref)
   (list-ref the-store ref))
(define test
  (list
    (s-statement           
     (assignment-statement
      'a
      (disjunction-exp
       (simple-disjunct
        (simple-conjunct
         (comparison-inversion
          (simple-comp
           (simple-sum
            (simple-term
             (simple-factor
              (simple-power
               (atom-primary
                (number-atom
                 12)))))))))))))
    (s-statement
     (assignment-statement
      'b
      (disjunction-exp
       (simple-disjunct
        (simple-conjunct
         (comparison-inversion
          (simple-comp
           (addition-sum
            (simple-sum
             (simple-term
              (simple-factor
               (simple-power
                (atom-primary
                 (boolean-atom
                 'True)
                 )))))
            (simple-term
             (simple-factor
              (simple-power
               (atom-primary
                (number-atom
                 12)))))))))))))))
(define envtest (value-of-program test))
(value-of-thunk (second the-store) envtest)