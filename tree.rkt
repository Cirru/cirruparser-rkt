
#lang racket

(provide
  shorten
  append-item
  create-nesting
  resolve-dollar
  resolve-comma)

(define (shorten x)
  (if (list? x)
    (map shorten x)
    (hash-ref x 'text)))

(define (append-item xs level item)
  ; (displayln "\n\nappend-item: ")
  ; (display "xs:\t")
  ; (print (shorten xs))
  ; (display "\nlevel:\t")
  ; (displayln level)
  ; (display "item:\t")
  ; (displayln item)

  (define res (if (equal? level 0)
    (append xs (list item))
    (append (drop-right xs 1) (list
      (append-item (last xs) (- level 1) item)))))
  ; (display "\nresult:")
  ; (print (shorten res))
  res)

(define (nesting-helper xs n)
  (if (<= n 1) xs
    (list (nesting-helper xs (- n 1)))))

(define (create-nesting n)
  (nesting-helper (list) n))

(define (dollar-helper before after)
  (if (equal? (length after) 0) before
    (let
      ((cursor (first after)))
      (cond
        ((list? cursor)
          (dollar-helper
            (append before (list (resolve-dollar cursor)))
            (rest after)))
        ((equal? (hash-ref cursor 'text) "$")
          (append before (list (resolve-dollar (rest after)))))
        (else (dollar-helper
          (append before (list cursor)) (rest after)))))))

(define (resolve-dollar xs)
  (if (equal? (length xs) 0) xs
    (dollar-helper (list) xs)))

(define (comma-helper before after)
  ; (displayln "")
  ; (print (shorten before))
  ; (display "\t\t")
  ; (print (shorten after))

  (if (equal? (length after) 0) before
    (let
      ((cursor (first after)))
      (if (and (list? cursor) (> (length cursor) 0))
        (let
          ((head (first cursor)))
          (cond
            ((list? head)
              (comma-helper
                (append before (list (resolve-comma cursor)))
                (rest after)))
            ((equal? (hash-ref head 'text) ",")
              (comma-helper before
                (append (resolve-comma (rest cursor)) (rest after))))
            (else
              (comma-helper
                (append before (list (resolve-comma cursor)))
                (rest after)))))
        (comma-helper (append before (list cursor))
          (rest after))))))

(define (resolve-comma xs)
  (if (equal? (length xs) 0 ) xs
    (comma-helper (list) xs)))
