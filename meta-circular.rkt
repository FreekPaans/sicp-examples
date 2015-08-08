#lang racket

(require "table.rkt")

(define (self-evaluating-exp? exp)
  (or (number? exp) (symbol? exp) (boolean? exp) (string? exp)))

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
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp) (cadddr exp))
  (define (true? val) (if val #t #f))
  (if (true? (eval-exp (if-predicate exp)))
      (eval-exp (if-consequent exp))
      (eval-exp (if-alternative exp))))

(define (if-exp? exp)
  (and (pair? exp) (eq? (car exp) 'if)))

(define (quoted-exp? exp)
  (and (pair? exp) (eq? (car exp) 'quote)))

(define (begin-exp? exp)
  (and (pair? exp) (eq? (car exp) 'begin)))

(define (eval-sequence exp)
  (define (last-exp? exps) (null? (cdr exps)))
  (define (first-exp exps) (car exps))
  (define (remaining-exps exps) (cdr exps))
  (define (iter-seq exps)
    (cond ((last-exp? exps) (eval-exp (first-exp exps)))
          (else
           (eval-exp (first-exp exps))
           (iter-seq (remaining-exps exps)))))
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
  

(eval-exp '(if (> 4 3) (begin (display "true!") #t) (begin (display "false!!!..") #f)))
