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

    [sim-statement (identifier "=" expr) assignment-statement]
    [sim-statement ("return" expr) return-statement]
    [sim-statement ("global" identifier) global-statement]
    [sim-statement ("pass") pass-statement]
    [sim-statement ("break") break-statement]
    [sim-statement ("continue") continue-statement]
    [sim-statement ("print" expr) print-statement]

    [parameter (identifier "=" expr) param-with-default]

    [com-statement ("def" identifier "(" (arbno parameter ",") ")" ":" (arbno statements)) function-def-statement]
    [com-statement ("if"  expr ":" (arbno statements) "else" ":" (arbno statements)) if-statement]
    [com-statement ("for" identifier "in" expr ":" (arbno statements)) for-statement]

    [expr (disjunct) disjunction-exp]

    [disjunct (conjunct) simple-disjunct]
    [disjunct (disjunct "or" conjunct) compound-disjunct]

    ;[conjunct (inversion) simple-conjunct]
    [conjunct (conjunct "and" inversion) compound-conjunct]

    [inversion ("not" inversion) not-inversion]
    [inversion (comparison) comparison-inversion]

    ;[comparison (sum) simple-comp]
    [comparison (sum (arbno comp-op-sum-pair)) compound-comp]

    [comp-op-sum-pair ("==" sum) eq-sum]
    [comp-op-sum-pair (">"  sum) gt-sum]
    [comp-op-sum-pair ("<"  sum) lt-sum]

    [sum (sum "+" term) addition-sum]
    [sum (sum "-" term) subtraction-sum]
    ;[sum (term) simple-sum]

    [term (term "*" factor) multiplication-factor]
    [term (term "/" factor) division-factor]
    ;[term (factor) simple-term]

    [factor ("+" factor) plus-factor]
    [factor ("-" factor) minus-factor]
    [factor (power) simple-factor]

    [power (atom "**" factor) to-power]
    [power (primary) simple-power]

    ;[primary (atom) atom-primary]
    [primary (primary "[" expr "]") expression-primary]
    [primary (primary "(" ")") empty-primary]
    [primary (primary "(" (arbno expr ",") ")") argument-primary]

    [atom (identifier) id-atom]
    [atom ("False") boolean-atom]
    [atom ("True")  boolean-atom]
    [atom (number) number-atom]
    [atom ("none") none-atom]
    [atom ("[" (arbno expr ",") "]") list-atom]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))