#lang racket

(require "table.rkt")

(provide env-has-variable?)
(provide env-get-variable-value)
(provide make-env)



(define (env-has-variable? name env)
  (env 'has-variable? name))

(define (env-get-variable-value name env)
  (env 'get-variable name))

(define (make-env parent-env)
  (define data (make-table))

  (define (has-variable? name)
    (or
     (has-key? name data)
     (if (not (null? parent-env)) (parent-env 'has-variable? name) #f)))
     

  (define (set-variable! name value)
    (set! data (insert! name value data)))

  (define (get-variable name)
    (if (has-key? name data)
        (lookup name data)
        (parent-env 'get-variable name)))
  
  (define (dispatch msg . args)
    (cond
      ((eq? msg 'has-variable?) (has-variable? (car args)))
      ((eq? msg 'set-variable!) (set-variable! (car args) (cadr args)))
      ((eq? msg 'get-variable) (get-variable (car args)))
      (else
       (error "unknown msg " msg))))
  dispatch)