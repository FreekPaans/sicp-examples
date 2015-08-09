#lang racket

(require "table.rkt")

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
(define (compound-procedure? exp) (and (pair? exp) (eq? (car exp) 'lambda)))
(define (compound-procedure-body exp) (caddr exp))
(define (compound-procedure-env exp) (cadddr exp))
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

(define (apply-exp operator operands env)
  (cond
    ((compound-procedure? operator)
     (begin
       (define new-env (make-env (compound-procedure-env operator)))
       (eval-sequence (compound-procedure-body operator) new-env)))
    (else
     (apply operator operands))))

(define (eval-if exp)
  (if (true? (eval-exp (if-predicate exp)))
      (eval-exp (if-consequent exp))
      (eval-exp (if-alternative exp))))

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

(define (eval-define name value env)
  (env 'set-variable! name value))

(define (env-has-variable? name env)
  (env 'has-variable? name))

(define (env-get-variable-value name env)
  (env 'get-variable name))
     
(define (lookup-variable name env)
  (cond ((env-has-variable? name env) (env-get-variable-value name env))
        (else
         (error "unknown variable " name))))

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

(define (eval-lambda params body env)
  (list 'lambda params body env))



(define (eval-exp exp env)
  (cond
    ((self-evaluating-exp? exp) exp)
    ((variable? exp) (lookup-variable (variable-name exp) env))
    ((if-exp? exp) (eval-if exp))
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
(global-env 'set-variable! 'display display)
(global-env 'set-variable! 'newline newline)

(eval-exp
 '
 (begin
   (define y 1)
   (define x (lambda ()
               (define y 5)
               (+ y 2)))
   (display (x))
   (newline)
   (display y))
 global-env)
