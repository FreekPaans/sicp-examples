#lang racket

(require "table.rkt")

(define (self-evaluating-exp? exp)
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

(define (apply-exp operator operands)
  (apply (get-primitive-proc operator) operands))

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

(define (eval-sequence exp)
  (define (iter-seq items)
    (cond ((null? (cdr items)) (eval-exp (car items)))
          (else
           (eval-exp (car items))
           (iter-seq (cdr items)))))
  (iter-seq exp))

(define (begin-actions exp)
  (cdr exp))

(define (application? exp)
  #t)

(define (list-of-values exps)
  (map eval-exp exps))

(define (eval-exp exp)
  (cond
    ((self-evaluating-exp? exp) exp)
    ((if-exp? exp) (eval-if exp))
    ((begin-exp? exp) (eval-sequence (begin-actions exp)))
    ((application? exp)
     (apply-exp (eval-exp (operator exp)) (list-of-values (operands exp))))
    (else (error "unknown expression " exp))))
  

(eval-exp '(begin (display 1) (display 2) 3))
