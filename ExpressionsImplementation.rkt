#lang racket
(require (lib "eopl.ss" "eopl"))
(require "./DataTypesDefinition.rkt")
(require "./lexer.rkt")
(require "./parser.rkt")


(define (extend-env-with-args params args env prev-env)
  (if (null? args)
      (extend-env-only-params params env prev-env)
      (extend-env-with-args (rest params) (rest args)
                            (extend-env-with-arg (first params) (first args) env prev-env)
                            prev-env)
      )
  )
(define (extend-env-with-arg param1 arg1 env prev-env)
  (cases param param1
    (param-with-default (id exp1)
                        (extend-environment id (newref (a-thunk arg1 prev-env)) env))
    )
  )
(define (extend-env-only-params params env prev-env)
  (if (null? params)
      env
      (cases param (first params)
        (param-with-default (id exp1)
                            (extend-env-only-params (rest params) (extend-environment id (newref (a-thunk exp1 prev-env)) env) prev-env))
        )
      )
  )

(define (value-of-program program)
  (begin
    (define env (empty-environment))
    (value-of-statements program env)
    )
 )

(define (value-of-statements statements env)
  (if (null? statements)
      env
      (let ((result (value-of-statement (first statements) env)))
        (if (null? result)
            (value-of-statements (rest statements) result)
            (cond
              [(eqv? 'Return (first result))
               result]
              [else
               (value-of-statements (rest statements) result)])
            )
        )
      )
  )
(define (value-of-statements-function statements env)
  (let ((result (value-of-statements statements env)))
    (if (null? result)
        (non-val)
        (if (eqv? 'Return (first result))
            (second result) 
            (non-val)
            )
        )
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
                       env))
    (printval-statement (lst)
                     (begin
                       (display (map (lambda (x) (value-of-atom x env)) lst))
                       env))
    (evaluate-statement (address)
                        (let*
                          ((program (open-input-file "./test.plpy"))
                           (result (value-of-program (parse program))))
                          (begin
                            (display result)
                            env)))
    )
  )

(define (value-of-c-statement s env)
  (cases compound-statement s
    (function-def-statement (id params body)
                            (value-of-function-def-statement id params body env))
    (if-statement (condition body else-body)
                  (value-of-if-statement condition body else-body env value-of-statements))
    (for-statement (id iterable body)
                   (value-of-for-statement id iterable body env)
                   )
    )
  )

(define (value-of-assignment-statement id right-hand env)
  (let ((prev-dec (apply-environment id env)))
    (if (null? prev-dec)
        (let ((new-ref (newref (a-thunk right-hand env))))
          (extend-environment id new-ref env)
          )
        (setref! prev-dec (a-thunk right-hand env))
        )
    )
  )
(define (value-of-return-statement body env)
  (if (null? body)
      (list 'Return (non-val))
      (list 'Return (value-of-expression body env))
      )
  )

(define (value-of-global-statement id env)
  (let ((global-ref (apply-environment id (get-global-environment env))))
    (extend-environment id global-ref env)
    )
  )
(define (value-of-function-def-statement id params body env)
  (let ((new-ref (newref (proc-val (procedure id params body)))))
    (extend-environment id new-ref env)
    )
  )
(define (value-of-if-statement condition body else-body env f)
  (let ((condition-result (value-of-expression condition env)))
    (if (expval->val condition-result)
        (f body env)
        (f else-body env)
        )
    )
  )
(define (value-of-for-statement id iterable body env)
  (let ((iterable-value (reverse (expval->val (value-of-expression iterable env)))))
        (let ((result (value-of-for-body id iterable-value body env)))
          (if (eqv? result 'break)
              env
              result)
          )
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
            [(null? result)
             (value-of-statements-inside-for body (extend-environment id (newref (a-thunk (first iterable) env)) env))]
            [(eqv? 'Return (first result))
             result]
            [else
             (value-of-statements-inside-for body (extend-environment id (newref (a-thunk (first iterable) env)) env))]
            )
          )
        )
      )
  )
(define (value-of-statements-inside-for statements env)
  (if (null? statements)
      env
      (let ((to-execute (first statements)))
        (cases statement to-execute
          (s-statement (s)
                       (cases simple-statement s
                         (break-statement ()
                                          'break)
                         (continue-statement ()
                                             env)
                         (return-statement (e1)
                                           (value-of-return-statement e1 env))
                         (else
                          (begin
                            (value-of-statements-inside-for (rest statements) (value-of-statement to-execute env))
                          env))
                         )
                       )
          (c-statement (s)
                       (cases compound-statement s
                         (if-statement (condition body else-body)
                                       (value-of-if-statement condition body else-body env value-of-statements-inside-for))
                         (else
                          (begin
                            (value-of-statements-inside-for (rest statements) (value-of-statement to-execute env))
                          env))
                         )
                       )
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
                       (let ((val-x1 (value-of-disjunction x1 env))
                             (val-x2 (value-of-conjunction x2 env)))
                         (if (expval->val val-x1)
                             val-x1
                             val-x2)))))

(define (value-of-conjunction body env)
  (cases conjunct body
    (simple-conjunct (x)
                     (value-of-inversion x env))
    (compound-conjunct (x1 x2)
                       (let ((val-x1 (value-of-conjunction x1 env))
                             (val-x2 (value-of-inversion x2 env)))
                         (if (not (expval->val val-x1))
                             val-x1
                             val-x2)))))

(define (value-of-inversion body env)
  (cases inversion body
    (not-inversion (x)
                   (bool-val (not (expval->val (value-of-inversion x env)))))
    (comparison-inversion (comp)
                           (value-of-comparison comp env))))

(define (value-of-comparison body env)
  (cases comparison body
    (simple-comp (x)
                 (value-of-sum x env))
    (compound-comp (x1 x2)
                   (value-of-comp-op-sum-pairs (value-of-sum x1 env) x2 env))))
(define (extract-sum cosp)
  (cases comp-op-sum-pair cosp
    (eq-sum (s) s)
    (lt-sum (s) s)
    (gt-sum (s) s)))

(define (value-of-comp-op-sum-pairs precursor body env)
  (cond
    [(null? body) (bool-val #t)]
    [else (let ((result (expval->val (value-of-comp-op-sum-pair precursor (first body) env))))
             (bool-val (and (expval->val (value-of-comp-op-sum-pairs (value-of-sum (extract-sum (first body)) env) (rest body) env)) result)))]))

(define (value-of-comp-op-sum-pair precursor body env)
  (cases comp-op-sum-pair body
    (eq-sum (x) (bool-val (equal? (expval->val precursor) (expval->val (value-of-sum x env)))))
    (lt-sum (x) (bool-val (< (expval->val precursor) (expval->val (value-of-sum x env)))))
    (gt-sum (x) (bool-val (> (expval->val precursor) (expval->val (value-of-sum x env)))))))

(define (value-of-sum body env)
  (cases sum body
    (addition-sum (left-hand right-hand) (add-or (value-of-sum left-hand env)  (value-of-term right-hand env) env))
    (subtraction-sum (left-hand right-hand)
                     (num-val (- (expval->val (value-of-sum left-hand env)) (expval->val (value-of-term right-hand env)))))
    (simple-sum (x) (value-of-term x env))))

(define (add-or left-hand right-hand env)
  (cases expval left-hand
    (num-val (num) (num-val (+ (expval->val right-hand) num)))
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
    (minus-factor (x) (num-val (- (expval->val (value-of-factor x env)))))
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
                 (if (thunk? value)
                     (cases thunk value
                       (a-thunk (exp1 prev-env)
                                (let ((result (value-of-expression exp1 prev-env)))
                                  (setref! ref result)
                                  result)))
                     value))))
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
  (let ((val (expval->val (value-of-primary x env))))
    (cases proc val
      (procedure (id params body)
                 (apply-procedure id params body (list) (add-environment-scope env) env)
                 )
      )
    )
  )
(define (apply-procedure id params body args env prev-env)
  (value-of-statements-function body (extend-environment id
                                                (newref (proc-val (procedure id params body)))
                                                (extend-env-with-args params args env prev-env)))
  )
  
(define (value-of-argument-primary x args env)
  (let ((val (expval->val (value-of-primary x env))))
    (cases proc val
      (procedure (id params body)
                 (apply-procedure id params body args (add-environment-scope env) env)
                 )
      )
    )
  )
(define (value-of-thunk t)
  (cases thunk t
    (a-thunk (exp1 prev-env)
     (value-of-expression exp1 prev-env)
     )
    )
  )


(define (make-num num) (simple-sum
               (simple-term
                (simple-factor
                 (simple-power
                  (atom-primary
                   (number-atom
                    num)))))))

(define (deref ref)
   (list-ref the-store ref))
(define (sne num)
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
             num))))))))))))

(define for-list
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
            (list-atom
             (list
              (sne 1)
              (sne 2)
              (sne 3)
              (sne 4))))))))))))))

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
             (plus-factor
              (simple-factor
               (simple-power
                (atom-primary
                 (number-atom
                  155))))))))))))))
   (c-statement
    (function-def-statement
     'p1
     (list)
     (list
      (s-statement
       (global-statement
        'a
        )
       )
      (c-statement
       (for-statement
        'i
        for-list
        (list
         (c-statement
          (if-statement
           (disjunction-exp
            (simple-disjunct
             (simple-conjunct
              (comparison-inversion
               (compound-comp
                (simple-sum
                 (simple-term
                  (simple-factor
                   (simple-power
                    (atom-primary
                   (id-atom
                    'i))
                    ))))
                (list
                 (eq-sum
                  (make-num 2))))))))
           (list
            (s-statement
             (assignment-statement
              'b
              (disjunction-exp
               (simple-disjunct
                (simple-conjunct
                 (comparison-inversion
                  (simple-comp
                   (simple-sum
                    (simple-term
                     (plus-factor
                      (simple-factor
                       (simple-power
                        (atom-primary
                         (number-atom
                          170))))))))))))))
            (s-statement
             (return-statement (sne 77)))
            (s-statement
             (print-statement (list 123123))))
           (list
          (s-statement
           (print-statement (list 12)))))))
        )
       )
      )
     )
    )
    (c-statement
     (if-statement
      (disjunction-exp
       (simple-disjunct
        (simple-conjunct
         (comparison-inversion
          (compound-comp
           (simple-sum
            (simple-term
             (simple-factor
              (simple-power
               (empty-primary
                (atom-primary
                 (id-atom
                  'p1)))))))
              (list
               (eq-sum
                (make-num 77))))))))
         (list
          (s-statement
           (print-statement (list 'True))))
         (list
          (s-statement
           (print-statement (list 'False))))))
   
               
    (c-statement
     (for-statement
      'i
      for-list
      (list
       (c-statement
        (if-statement
         (disjunction-exp
          (simple-disjunct
           (simple-conjunct
            (comparison-inversion
             (compound-comp
              (simple-sum
               (simple-term
                (simple-factor
                 (simple-power
                  (atom-primary
                   (id-atom
                   'a))
                  ))))
              (list
               (eq-sum
                (make-num 155))))))))
         (list
          (s-statement
           (print-statement (list 123123))))
         (list
          (s-statement
           (print-statement (list 12)))))))
        )
       )
      )
     )

(define addr "'./test.pypl'")
(define test1 (list (s-statement (evaluate-statement (substring addr 1 (- (string-length addr) 1))))))

;(define envtest (value-of-program test1))
;;(value-of-thunk (third the-store) envtest)
(provide value-of-program)