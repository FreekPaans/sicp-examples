#lang racket

(require "table.rkt")

(define (primitive-exp? exp)
  (or (number? exp) (symbol? exp) (boolean? exp)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (get-primitive-proc symbol)
  (cond
    ((eq? '+ symbol) +)
    ((eq? '* symbol) *)
    ((eq? '/ symbol) /)
    ((eq? '- symbol) -)
    ((eq? '> symbol) >)
    ((eq? 'display symbol) display)
    (else (error "unknown symbol " symbol))))

(define (combination? exp)
  (pair? exp))

(define (apply-exp exp)
  (apply (get-primitive-proc (eval-exp (operator exp))) (map eval-exp (operands exp))))

(define (eval-if exp)
  (let ((condition (cadr exp))
        (on-true (caddr exp))
        (on-false (cadddr exp)))
    (if (eval-exp condition)
        (eval-exp on-true)
        (eval-exp on-false))))

(define (if-exp? exp)
  (and (pair? exp) (eq? (car exp) 'if)))

(define (quoted-exp? exp)
  (and (pair? exp) (eq? (car exp) 'quote)))

(define (begin-exp? exp)
  (and (pair? exp) (eq? (car exp) 'begin)))

(define (eval-begin exp)
  (define (iter-seq items)
    (cond ((null? (cdr items)) (eval-exp (car items)))
          (else
           (eval-exp (car items))
           (iter-seq (cdr items)))))
  (iter-seq (cdr exp)))

(define (eval-exp exp)
  (cond
    ((primitive-exp? exp) exp)
    ((if-exp? exp) (eval-if exp))
    ((begin-exp? exp) (eval-begin exp))
    (#t (apply-exp exp))
    (else (error "unknown expression " exp))))
  

(eval-exp '(begin (display 1) (display 2) 3))
