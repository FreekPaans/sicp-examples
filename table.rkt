#lang racket

(define (make-table)
  '())

(define (lookup key table)
  (let ((value (assoc key table)))
    (if value
        (cdr value)
        #f)))

(define (insert! key value table)
  (define (remove-key key table)
    (cond ((null? table) table)
          ((eq? (caar table) key) (cdr table))
          (else (remove-key key table))))
  (let ((current-value (assoc key table)))
    (if (not current-value)
        (cons (cons key value) table)
        (cons (cons key value) (remove-key key table)))))
        