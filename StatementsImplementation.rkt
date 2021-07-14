#lang racket




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
                   (value-of-for-statement id iterable body env))
    )
  )

    
                     