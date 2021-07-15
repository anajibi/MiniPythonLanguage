#lang racket
(require (lib "eopl.ss" "eopl"))


(define (value-of-program program)
  (begin
    (define env (empty-environment))
    (value-of-statements program env)
    )
 )

(define (value-of-statements statements env)
  (if (null? statements)
      env
      (value-of-statements (rest statements) (value-of-statement (first statements)))
      )
  )

(define (value-of-statement s env)
  (cases simple-statement s
    (value-of-s-statement s env)
    (else (value-of-c-statement s env))
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
  (let ((new-ref (newref (thunk right-hand))))
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
  (let ((new-ref (newref (proc-val (procedure params body env)))))
    (extend-environment id new-ref env)
    )
  )
(define (value-of-if-statement condition body else-body env)
  (let ((condition-result (value-of-expression condition env)))
    (if (expval->boolean condition-result)
        (value-of-statements body env)
        (value-of-statements else-body env)
        )
    )
  )
(define (value-of-for-statement id iterable body env)
  (let ((iterable-value (reverse (expval->list (value-of-expression iterable env)))))
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
        (cases to-execute simple-statement
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

(define (value-of-print-statement statement env)
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
             (newline)]))

(define (value-of-evaluate-statemenet path)
  (run (call-with-input-file path (lambda (in) (port->string in)))))
  


    
                     