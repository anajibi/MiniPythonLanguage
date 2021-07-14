#lang eopl

(load-relative "./DataTypesDefinition.rkt")
(load-relative "./StatementsImplementation.rkt")

(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define the-grammar
  '([simple-statement (identifier "=" expression) assignment-statement]
    [simple-statement ("return" expression) return-statement]
    [simple-statement ("global" identifier) global-statement]
    [simple-statement ("pass") pass-statement]
    [simple-statement ("break") break-statement]
    [simple-statement ("continue") continue-statement]
    [simple-statement ("print" expression) print-statement]

    [compound-statement ("def" identifier "(" (separated-list identifier ",") ")" ":" (separated-list simple-statement ";")) function-def-statement]
    [compound-statement ("if"  expression ":" (separated-list simple-statement ";") "else" ":" (separated-list simple-statement ";")) if-statement]
    [compound-statement ("for" identifier "in" expression ":"(separated-list simple-statement ";")) for-statement]

    [inversion ("not" inversion) not-inversion]
    [disjunct (disjunct "or" conjunct) compound-disjunct]
    [conjunct (conjunct "and" inversion) compound-conjunct]

    [comp-op-sum-par ("==" sum) eq-sum]
    [comp-op-sum-par ("<" sum) lt-sum]
    [comp-op-sum-par (">" sum) gt-sum]

    [sum (sum "+" term) addition-sum]
    [sum (sum "-" term) subtraction-sum]
    [sum (term) simple-sum]

    [term (term "*" factor) multiplication-factor]
    [term (term "/" factor) division-factor]
    [term (factor) simple-term]


    [factor ("+" factor) plus-factor]
    [factor ("-" factor) minus-factor]
    [factor (power) simple-factor]

    [power (atom "**" factor) to-power]
    [power (primary) simple-power]

    [primary (atom) atom-primary]
    [primary (primary "[" expression "]") expression-primary]
    [primary (primary "(" ")") empty-primary]
    [primary (primary "(" (separated-list expression ",") ")") argument-primary]

    [atom id-atom]
    [atom (identifier) boolean-atom]
    [atom ("None") none-atom]
    [atom (number) number-atom]
    [atom (list) list-atom]
   ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      [a-program (statement) (value-of-statement statement (empty-env))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide run)
