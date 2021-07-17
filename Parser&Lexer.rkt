#lang eopl

;; Grammar.

(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define the-grammar
  '([program (statements) statement]
    [statements (sim-statement ";") s-statement]
    [statements (com-statement ";") c-statement]
    [sim-statement (identifier "=" expression) assignment-statement]
    [sim-statement ("return" expression) return-statement]
    [sim-statement ("global" identifier) global-statement]
    [sim-statement ("pass") pass-statement]
    [sim-statement ("break") break-statement]
    [sim-statement ("continue") continue-statement]
    [sim-statement ("print" expression) print-statement]

    [com-statement ("def" identifier "(" (separated-list identifier ",") ")" ":" (arbno statements)) function-def-statement]
    [com-statement ("if"  expression ":" (arbno statements) "else" ":" (arbno statements)) if-statement]
    [com-statement ("for" identifier "in" expression ":" (arbno statements)) for-statement]

    [expression (number) number-atom]
    ;[expression (bool) boolean-atom]
    [expression ("+" "(" expression "," expression ")") add-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("*" "(" expression "," expression ")") multiply-exp]
    [expression ("not" "(" expression ")") not-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression (identifier) var-exp]
    [expression ("let" (arbno identifier "=" expression) "in" expression) let-exp]
    [expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp]
    [expression ("begin" expression (arbno ";" expression) "end") begin-exp]
    [expression ("set" identifier "=" expression) assign-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))