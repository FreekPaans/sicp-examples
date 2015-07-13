#lang racket

;begin rectangular

(define (install-rectangular-package tagname)
  (define (real-part-rect z)
    (car z))

  (define (imag-part-rect z)
    (cdr z))

  (define (magnitude-rect z)
    (define (square x) (* x x))
    (sqrt (+ (square (real-part-rect z)) (square (imag-part-rect z)))))

  (define (angle-rect z)
    (atan (imag-part-rect z) (real-part-rect z)))

  (define (make-from-real-imag-rect x y)
    (cons x y))

  (define (make-from-mag-ang-rect r a)
    ((make-from-real-imag-rect (* r (cos a)) (* r (sin a)))))

  (install-operation tagname 'real-part real-part-rect)
  (install-operation tagname 'imag-part imag-part-rect)
  (install-operation tagname 'angle angle-rect)
  (install-operation tagname 'magnitude magnitude-rect)
  (install-operation tagname 'make-from-real-imag make-from-real-imag-rect)
  (install-operation tagname 'make-from-mag-ang make-from-mag-ang-rect) 
  'done)

(define rect-tag 'rect)

;end rectangular

;begin polar

(define (install-polar-package tagname)
  (define (real-part-polar z)
    (* (magnitude-polar z) (cos (angle-polar z))))
  
  (define (imag-part-polar z)
    (* (magnitude-polar z) (sin (angle-polar z))))
  
  (define (magnitude-polar z)
    (car z))
  
  (define (angle-polar z)
    (cdr z))
  
  (define (make-from-real-imag-polar x y)
    (define (square x) (* x x))
    (make-from-mag-ang-polar (sqrt (+ (square x) (square y))) (atan y x)))
  
  (define (make-from-mag-ang-polar r a)
    (cons r a))
  (install-operation tagname 'real-part real-part-polar)
  (install-operation tagname 'imag-part imag-part-polar)
  (install-operation tagname 'angle angle-polar)
  (install-operation tagname 'magnitude magnitude-polar)
  (install-operation tagname 'make-from-real-imag make-from-real-imag-polar)
  (install-operation tagname 'make-from-mag-ang make-from-mag-ang-polar) 
  'done)

(define polar-tag 'polar)

;end polar

;begin tagging

(define (add-tag tag val)
  (cons tag val))

(define (is-tag? tag val)
  (cond ((not (tagged-value? val)) #f)
        (else (eq? (car val) tag))))

(define (get-tag val)
  (cond ((not (tagged-value? val)) (error "not a tagged value " val))
        (else (car val))))

(define (tagged-value? val)
  (and (pair? val) (symbol? (car val))))

(define (unwrap-tag val)
  (cond ((not (tagged-value? val)) (error "not a tagged value " val))
        (else (cdr val))))

;end tagging

;defaults

(define (make-from-real-imag x y)
  (add-tag rect-tag ((find-op-for-tag 'make-from-real-imag rect-tag) x y)))

(define (make-from-mag-ang r a)
  (add-tag polar-tag ((find-op-for-tag 'make-from-mag-ang polar-tag) r a)))

;end defaults

;begin optable

(define ops '())

(define (find-ops-for-tag tagname)
  (define (iter ops)
    (cond ((null? ops) #f)
          ((eq? (caar ops) tagname) (cdar ops))
          (else (iter (cdr ops)))))
  (iter ops))

(define (find-op-for-tag opname tagname)
  (define (find-op ops)
    (cond ((null? ops) #f)
          ((eq? (caar ops) opname) (cdar ops))
          (else (find-op (cdr ops)))))
  (let ((ops (find-ops-for-tag tagname)))
    (if (not ops) #f
        (find-op  ops))))

(define (install-operation tag-name op-name procedure)
  (define (remove-ops-for-tag tagname ops)
    (define (iter ops)
      (cond ((null? ops) '())
            ((eq? (caar ops) tagname) (cdr ops))
            (else (cons (car ops) (iter (cdr ops))))))
    (iter ops))
  (let ((ops-for-tag (find-ops-for-tag tag-name)))
    (if ops-for-tag
        (set! ops (cons (append (list tag-name) ops-for-tag (list (cons op-name procedure))) (remove-ops-for-tag tag-name ops)))
        (begin
          (set! ops (cons (cons tag-name '()) ops))
          (install-operation tag-name op-name procedure)))))
  
(define (apply-operation op-name arg)
  (let ((op (find-op-for-tag op-name (get-tag arg))))
    (if op (op (unwrap-tag arg))
        (error "couldn't find op " op-name " for tag " (get-tag arg)))))

;end table

;begin globals

(define (imag-part z)
  (apply-operation 'imag-part z))

(define (real-part z)
  (apply-operation 'real-part z))

(define (magnitude z)
  (apply-operation 'magnitude z))

(define (angle z)
  (apply-operation 'angle z))

;end complex number dispatch

;begin complex number ops

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


;end complex number ops

(install-rectangular-package rect-tag)
(install-polar-package polar-tag)