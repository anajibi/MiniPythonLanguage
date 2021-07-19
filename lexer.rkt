#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require racket/pretty)

(define simple-python-lexer
           (lexer
            (";" (token-semicolon))
            ("=" (token-assignto))
            ("+" (token-plus))
            ("-" (token-minus))
            ("*" (token-multiply))
            ("/" (token-divide))
            ("**" (token-power))
            ("==" (token-equals))
            ("<" (token-lessthan))
            (">" (token-greaterthan))
            ("(" (token-opening-paranthesis))
            (")" (token-closing-paranthesis))
            ("[" (token-opening-bracket))
            ("]" (token-opening-bracket))
            (":" (token-colon))
            ("," (token-comma))
            ("pass" (token-pass))
            ("break" (token-break))
            ("continue" (token-continue))
            ("return" (token-return))
            ("global" (token-global))
            ("print" (token-print))
            ("def" (token-def))
            ("if" (token-IF))
            ("else" (token-ELSE))
            ("for" (token-FOR))
            ("in" (token-IN))
            ("or" (token-OR))
            ("and" (token-AND))
            ("not" (token-NOT))
            ("True" (token-TRUE))
            ("False" (token-FALSE))
            ("None" (token-NONE))
            ((:or (:+ (char-range #\0 #\9))
                  (:: (:+ (char-range #\0 #\9)) #\.
                      (:+ (char-range #\0 #\9))))
             (token-NUM (string->number lexeme)))
            ((::
              (:or (char-range #\a #\z)
                   (char-range #\A #\Z)
                   #\_)
              (:*
               (:or (char-range #\a #\z)
                    (char-range #\A #\Z)
                    (char-range #\0 #\9)
                    #\_)))
             (token-ID lexeme))
            (whitespace (simple-python-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (NUM ID))
(define-empty-tokens b (EOF
                        semicolon
                        assignto
                        plus
                        minus
                        multiply
                        divide
                        power
                        equals
                        lessthan
                        greaterthan
                        opening-paranthesis
                        closing-paranthesis
                        opening-bracket
                        closing-bracket
                        colon
                        comma
                        pass
                        break
                        continue
                        return
                        global
                        print
                        def
                        IF
                        ELSE
                        FOR
                        IN
                        OR
                        AND
                        NOT
                        TRUE
                        FALSE
                        NONE))


;test
(define test-program (open-input-file "./test.plpy"))
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-python-lexer test-program))
(define (lex-all my-lexer)
  (let ((lex-res (my-lexer)))
    (if (equal? 'EOF lex-res)
        (list lex-res)
        (cons lex-res (lex-all my-lexer)))))

(define test-lex-output (open-output-file "./testlex.txt" #:exists 'replace))
(pretty-print (lex-all my-lexer) test-lex-output)
(close-output-port test-lex-output)

(provide a)
(provide b)
(provide simple-python-lexer)
