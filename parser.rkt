#lang racket

(require "tree.rkt")

(provide cirru-parser-parse cirru-parser-pare)

(define (cirru-parser-parse code filename)
  (define buffer (make-hash))

  (define state (make-hash))
  (hash-set! state 'name 'indent)
  (hash-set! state 'x 1)
  (hash-set! state 'y 1)
  (hash-set! state 'level 1)
  (hash-set! state 'indent 0)
  (hash-set! state 'indented 0)
  (hash-set! state 'nest 0)
  (hash-set! state 'path filename)

  (define res (parse (list) buffer state code))
  (set! res (map resolve-dollar res))
  (set! res (map resolve-comma res))
  res)

(define (cirru-parser-pare code filename)
  (shorten (cirru-parser-parse code filename)))

(define (shorten x)
  (if (list? x)
    (map shorten x)
    (hash-ref x 'text)))

(define (escape-eof xs buffer state code)
  (error "EOF in escape state"))

(define (string-eof xs buffer state code)
  (error "EDOT in string state"))

(define (space-eof xs buffer state code)
  xs)

(define (token-eof xs buffer state code)
  xs)

;; escape

(define (escape-newline xs buffer state code)
  (error "new line while escape"))

(define (escape-n xs buffer state code)
  (hash-set! state 'x
    (+ (hash-ref state 'x)))
  (hash-set! buffer 'text
    (string-append (hash-ref buffer 'text) "\n"))
  (hash-set! state 'name 'string)
  (parse xs buffer state (substring code 1)))

(define (escape-t xs buffer state code)
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (hash-set! buffer 'text
    (string-append (hash-ref buffer 'text) "\n"))
  (hash-set! state 'name 'string)
  (parse xs buffer state (substring code 1)))

(define (escape-else xs buffer state code)
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (hash-set! buffer 'text
    (string-append (hash-ref buffer 'text)
      (substring code 0 1)))
  (hash-set! state 'name 'string)
  (parse xs buffer state (substring code 1)))

;; string

(define (string-backslash xs buffer state code)
  (hash-set! state 'name 'escape)
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (parse xs buffer state (substring code 1)))

(define (string-newline xs buffer state code)
  (error "newline in a string"))

(define (string-quote xs buffer state code)
  (hash-set! state 'name 'token)
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (parse xs buffer state (substring code 1)))

(define (string-else xs buffer state code)
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (hash-set! buffer 'text (substring code 0 1))
  (parse xs buffer state (substring code 1)))

;; space

(define (space-space xs buffer state code)
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (parse xs buffer state (substring code 1)))

(define (space-newline xs buffer state code)
  (if
    (not (equal? (hash-ref state 'nest) 0))
    (error "incorrect nesting")
    null)
  (hash-set! state 'name 'indent)
  (hash-set! state 'x 1)
  (hash-set! state 'y
    (+ (hash-ref state 'y) 1))
  (hash-set! state 'indented 0)
  (parse xs buffer state (substring code 1)))

(define (space-open xs buffer state code)
  (define nesting (create-nesting 1))
  (define xs (append-item xs (hash-ref state 'level) nesting))
  (hash-set! state 'nest
    (+ (hash-ref state 'nest) 1))
  (hash-set! state 'level
    (+ (hash-ref state 'level) 1))
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (parse xs buffer state (substring code 1)))

(define (space-close xs buffer state code)
  (hash-set! state 'nest
    (- (hash-ref state 'nest) 1))
  (hash-set! state 'level
    (- (hash-ref state 'level) 1))
  (if (< (hash-ref state 'nest) 0)
    (error "close at space") null)
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (parse xs buffer state (substring code 1)))

(define (space-quote xs buffer state code)
  (hash-set! state 'name 'string)

  (define buffer (make-hash))
  (hash-set! buffer 'text "")
  (hash-set! buffer 'x (hash-ref state 'x))
  (hash-set! buffer 'y (hash-ref state 'y))
  (hash-set! state 'x
    (+ (hash-ref state 'path) 1))
  (parse xs buffer state (substring code 1)))

(define (space-else xs buffer state code)
  (hash-set! state'name 'token)

  (define buffer (make-hash))
  (hash-set! buffer 'text (substring code 0 1))
  (hash-set! buffer 'x (hash-ref state 'x))
  (hash-set! buffer 'y (hash-ref state 'y))
  (hash-set! buffer 'path (hash-ref state 'path))
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (parse xs buffer state (substring code 1)))

;; token

(define (token-space xs buffer state code)
  (hash-set! state 'name 'space)
  (hash-set! buffer 'ex (hash-ref state 'x))
  (hash-set! buffer 'ey (hash-ref state 'y))
  (define xs (append-item xs (hash-ref state 'level) buffer))
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (define buffer (make-hash))
  (parse xs buffer state (substring code 1)))

(define (token-newline xs buffer state code)
  (hash-set! state 'name 'indent)
  (hash-set! buffer 'ex (hash-ref state 'x))
  (hash-set! buffer 'ey (hash-ref state 'y))
  (define xs (append-item xs (hash-ref state 'level) buffer))
  (hash-set! state 'indented 0)
  (hash-set! state 'x 1)
  (hash-set! state 'y
    (+ hash-ref state 'y) 1)
  (define buffer (make-hash))
  (parse xs buffer state (substring code 1)))

(define (token-open xs buffer state code)
  (error "open parenthesis in token"))

(define (token-close xs buffer state code)
  (hash-set! state 'name 'space)
  (hash-set! buffer 'ex (hash-ref state 'x))
  (hash-ref! buffer 'ey (hash-ref state 'y))
  (define xs (append-item xs (hash-ref state 'level buffer)))
  (define buffer (make-hash))
  (parse xs buffer state code))

(define (token-quote xs buffer state code)
  (hash-set! state 'name 'string)
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (parse xs buffer state (substring code 1)))

(define (token-else xs buffer state code)
  (hash-set! buffer 'text
    (string-append (hash-ref buffer 'text) (substring code 0 1)))
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (parse xs buffer state (substring code 1)))

;; indent

(define (indent-space xs buffer state code)
  (hash-set! state 'indented
    (+ (hash-ref state 'indented) 1))
  (hash-set! state 'x
    (+ (hash-ref state 'x) 1))
  (parse xs buffer state (substring code 1)))

(define (indent-newline xs buffer state code)
  (hash-set! state 'x 1)
  (hash-set! state 'y
    (+ (hash-ref state 'y) 1))
  (hash-set! state 'indented 0)
  (parse xs buffer state (substring code 1)))

(define (indent-close xs buffer state code)
  (error "close parenthesis at indent"))

(define (indent-else xs buffer state code)
  (hash-set! state 'name 'space)
  (if
    (equal? (remainder (hash-ref state 'indented) 2) 1)
    (error "odd indnetation") null)
  (define indented (remainder (hash-ref state 'indented 2)))
  (define diff (- indented (hash-ref state 'indent)))

  (define level
    (if (<= diff 0)
      (- (+ (hash-ref state 'level) diff) 1)
      (hash-ref state 'level)))
  (define nesting
    (if (<= diff 0)
      (create-nesting 1)
      (create-nesting diff)))
  (define xs (append-item xs level nesting))
  (hash-set! state 'level
    (+ (hash-ref state 'level) 1))
  (hash-set! state 'indented indented)
  (parse xs buffer state code))

(define (parse xs buffer state code)
  (define eof (equal? (string-length code) 0))
  (define char (if eof "" (substring code 0 1)))
  (define state-name (hash-ref state 'name))
  (define method (cond
    ((equal? state-name 'escape)
      (if eof escape-eof
        (cond
          ((equal? char "\n") escape-newline)
          ((equal? char "n") escape-n)
          ((equal? char "t") escape-t)
          (else escape-else))))

    ((equal? state-name 'string)
      (if eof string-eof
        (cond
          ((equal? char "\\") string-backslash)
          ((equal? char "\n") string-newline)
          ((equal? char "\"") string-quote)
          (else string-else) )))

    ((equal? state-name 'space)
      (if eof space-eof
        (cond
          ((equal? char " ") space-space)
          ((equal? char "\n") space-newline)
          ((equal? char "(") space-open)
          ((equal? char ")") space-close)
          ((equal? char "\"") space-quote)
          (else space-else) )))
    ((equal? state-name 'token)
      (if eof token-eof
        (cond
          ((equal? char " ") token-space)
          ((equal? char "\n") token-newline)
          ((equal? char "(") token-open)
          ((equal? char ")") token-close)
          ((equal? char "\"") token-quote)
          (else token-else))))
    ((equal? state-name 'indent)
      (if eof indent-close
        (cond
          ((equal? char " ") indent-space)
          ((equal? char "\n") indent-space)
          ((equal? char ")") indent-close)
          (else indent-else))))
))
  (method xs buffer state code))
