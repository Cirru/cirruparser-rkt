
#lang racket

(require cirruparser/parser)
(require json)

(define names (list
  ; "comma"
  "comma" "demo" "folding" "html" "indent"
  "line" "parentheses" "quote" "spaces" "unfolding"
  ))

(for-each
  (lambda (name)
    (begin
      (define file (format "examples/~a.cirru" name))
      (define json-file (format "ast/~a.json" name))
      (define tree (cirruparser-pare (file->string file) ""))
      (define formated (jsexpr->string tree))
      (define expected (jsexpr->string
        (string->jsexpr (file->string json-file))))
      (if (equal? formated expected)
        (displayln "ok")
        (displayln formated))
    ))
  names)
