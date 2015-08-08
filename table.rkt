#lang racket

(provide make-table)
(provide lookup)
(provide has-key?)
(provide insert!)

(define no-item 'no-such-item)

(define (make-table)
  '())

(define (has-key? key table)
  (let ((value (lookup key table)))
    (not (eq? value no-item))))

(define (lookup key table)
  (let ((value (assoc key table)))
    (if value
        (cdr value)
        no-item)))

(define (insert! key value table)
  (define (remove-key key table)
    (cond ((null? table) table)
          ((eq? (caar table) key) (cdr table))
          (else (remove-key key table))))
  (let ((current-value (assoc key table)))
    (if (not current-value)
        (cons (cons key value) table)
        (cons (cons key value) (remove-key key table)))))


(define (insert-2-dim! key1 key2 value table)
  (let ((current-value (lookup key1 table)))
    (if current-value
        (insert! key1 (insert! key2 value current-value) table)
        (insert! key1 (insert! key2 value (make-table)) table))))

(define (lookup-2-dim key1 key2 table)
  (let ((subtable (lookup key1 table)))
    (if (not subtable)
        #f
        (lookup key2 subtable))))

(define (make-mutable-table)
  (define the-table (make-table))
  (define (dispatch m)
    (cond ((eq? m 'lookup) (lambda (key1 key2) (lookup-2-dim key1 key2 the-table)))
          ((eq? m 'insert!) (lambda (key1 key2 value)
                              (set! the-table (insert-2-dim! key1 key2 value the-table))
                              the-table))
          (else (error "unknown msg " m))))
  dispatch)