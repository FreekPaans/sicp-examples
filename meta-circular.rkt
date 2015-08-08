#lang racket

(require "table.rkt")

(define (self-evaluating-exp? exp)
  (or (number? exp) (symbol? exp) (boolean? exp) (string? exp)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (combination? exp)
  (pair? exp))
(define (if-exp? exp)
  (and (pair? exp) (eq? (car exp) 'if)))
(define (quoted-exp? exp)
  (and (pair? exp) (eq? (car exp) 'quote)))
(define (begin-exp? exp)
  (and (pair? exp) (eq? (car exp) 'begin)))
(define (begin-actions exp)
  (cdr exp))
(define (application? exp)
  #t)
(define (first-operand exps)
  (car exps))
(define (no-operands? exps)
  (null? exps))
(define (remaining-operands exps)
  (cdr exps))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))
(define (true? val) (if val #t #f))
(define (last-exp? exps) (null? (cdr exps)))
(define (first-exp exps) (car exps))
(define (remaining-exps exps) (cdr exps))

(define (get-primitive-proc symbol)
  (cond
    ((eq? '+ symbol) +)
    ((eq? '* symbol) *)
    ((eq? '/ symbol) /)
    ((eq? '- symbol) -)
    ((eq? '> symbol) >)
    ((eq? 'display symbol) display)
    (else (error "unknown symbol " symbol))))

(define (apply-exp operator operands)
  (apply (get-primitive-proc operator) operands))

(define (eval-if exp)
  (if (true? (eval-exp (if-predicate exp)))
      (eval-exp (if-consequent exp))
      (eval-exp (if-alternative exp))))

(define (eval-sequence exp)
  (define (iter-seq exps)
    (cond ((last-exp? exps) (eval-exp (first-exp exps)))
          (else
           (eval-exp (first-exp exps))
           (iter-seq (remaining-exps exps)))))
  (iter-seq exp))

(define (list-of-values exps)
  (cond ((no-operands? exps) '())
        (else
         (let ((remaining (list-of-values (remaining-operands exps))))
           (cons (eval-exp (first-operand exps)) remaining )))))

(define (eval-exp exp)
  (cond
    ((self-evaluating-exp? exp) exp)
    ((if-exp? exp) (eval-if exp))
    ((begin-exp? exp) (eval-sequence (begin-actions exp)))
    ((application? exp)
     (apply-exp (eval-exp (operator exp)) (list-of-values (operands exp))))
    (else (error "unknown expression " exp))))
  

(eval-exp '(+  1 2))
;(eval-exp '(+ 1 2))

