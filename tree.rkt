
#lang racket

(provide append-item
  create-nesting
  resolve-dollar
  resolve-comma)

(define (append-item xs level item)
  (if (equal? level 0)
    (append xs (list item))
    (append (drop-right xs 1) (list
      (append-item (last xs) (- level 1) item)))))

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
            (append before (resolve-dollar cursor))
            (rest after)))
        ((equal? (hash-ref cursor 'text) "$")
          (append before (resolve-dollar (rest after))))
        (else (dollar-helper
          (append before (list cursor)) (rest after)))))))

(define (resolve-dollar xs)
  (if (equal? (length xs) 0) xs
    (dollar-helper (list) xs)))

(define (comma-helper before after)
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
                (append (resolve-comma (rest cursor))
                  (rest after))))
            (else (comma-helper
              (append before (list (resolve-comma cursor)))
              (rest after)))))
        (comma-helper (append before (list after))
          (rest after))))))

(define (resolve-comma xs)
  (if (equal? (length xs) 0 ) xs
    (comma-helper (list) xs)))
