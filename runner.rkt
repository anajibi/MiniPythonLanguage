#lang racket

(require "./ExpressionsImplementation.rkt")
(require "./lexer.rkt")
(require "./parser.rkt")
(require "./DataTypesDefinition.rkt")

(define (evaluate address)
  (value-of-program (parse (open-input-file address))))

(define program "./test.plpy")
;(define program (open-input-string "evaluate('./test.plpy');"))
;(define result (parse (open-input-file program)))
(define result (evaluate program))
(define test-output (open-output-file "./testresult.txt" #:exists 'replace))
(pretty-print result test-output)
(close-output-port test-output)
