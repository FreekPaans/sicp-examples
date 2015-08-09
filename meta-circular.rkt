#lang racket

(require "env.rkt")

(define (self-evaluating-exp? exp)
  (or (number? exp) (boolean? exp) (string? exp)))
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
(define (define-name exp)
  (cadr exp))
(define (define-value exp)
  (caddr exp))
(define (define? exp)
  (and (pair? exp) (eq? (car exp) 'define)))
(define (variable? exp)
  (symbol? exp))
(define (variable-name exp)
  exp)
(define (lambda-params exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (lambda? exp)
  (and (pair? exp) (eq? (car exp) 'lambda)))

(define (compound-procedure? exp) (and (pair? exp) (eq? (car exp) 'lambda)))
(define (compound-procedure-body exp) (caddr exp))
(define (compound-procedure-env exp) (cadddr exp))
(define (compound-procedure-param-names exp) (cadr exp))
(define (make-compound-procedure params body env)
  (list 'lambda params body env))


(define (apply-exp operator operands env)
  (cond
    ((compound-procedure? operator)
     (let ((new-env (make-env (compound-procedure-env operator)))
           (param-names (compound-procedure-param-names operator)))
       (if (not (= (length param-names) (length operands))) (error "arity mismatch, expected: " (length param-names) "given:" (length operands)) 'else)
       (define (iter-bind-params params operands)
         (cond ((null? params) 'done)
               (else
                (new-env 'set-variable! (car params) (car operands))
                (iter-bind-params (cdr params) (cdr operands)))))
       (iter-bind-params param-names operands)
       (eval-sequence (compound-procedure-body operator) new-env)))
    (else
     (apply operator operands))))

(define (eval-if exp env)
  (if (true? (eval-exp (if-predicate exp) env))
      (eval-exp (if-consequent exp) env)
      (eval-exp (if-alternative exp) env)))

(define (eval-sequence exp env)
  (define (iter-seq exps)
    (cond ((last-exp? exps) (eval-exp (first-exp exps) env))
          (else
           (eval-exp (first-exp exps) env)
           (iter-seq (remaining-exps exps)))))
  (iter-seq exp))

(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
        (else
         (let ((remaining (list-of-values (remaining-operands exps) env)))
           (cons (eval-exp (first-operand exps) env) remaining )))))
     
(define (lookup-variable name env)
  (cond ((env-has-variable? name env) (env-get-variable-value name env))
        (else
         (error "unknown variable " name))))
(define (eval-lambda params body env)
  (make-compound-procedure params body  env))

(define (eval-define name value env)
  (env 'set-variable! name value))

(define (eval-exp exp env)
  (cond
    ((self-evaluating-exp? exp) exp)
    ((variable? exp) (lookup-variable (variable-name exp) env))
    ((if-exp? exp) (eval-if exp env))
    ((begin-exp? exp) (eval-sequence (begin-actions exp) env))
    ((define? exp) (eval-define (define-name exp) (eval-exp (define-value exp) env) env))
    ((lambda? exp) (eval-lambda (lambda-params exp) (lambda-body exp) env))
    ((application? exp)
     (apply-exp (eval-exp (operator exp) env) (list-of-values (operands exp) env) env ) )
    (else (error "unknown expression " exp))))
  

(define global-env (make-env null))

(global-env 'set-variable! '+ +)
(global-env 'set-variable! '* *)
(global-env 'set-variable! '/ /)
(global-env 'set-variable! '- -)
(global-env 'set-variable! '> >)
(global-env 'set-variable! '= =)
(global-env 'set-variable! 'display display)
(global-env 'set-variable! 'newline newline)

(eval-exp
 '
 (begin
   (define fib
     (lambda (n)
       (if (= n 1)
           1
           (* n (fib (- n 1))))))
   (fib 5))
 global-env)
