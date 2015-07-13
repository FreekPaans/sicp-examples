#lang racket

;begin rectangular

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
  (make-from-real-imag-rect (* r (cos a)) (* r (sin a))))

;end rectangular

;begin polar

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

;end polar

;begin tagging

(define (add-tag tag val)
  (cons tag val))

(define (is-tag? tag val)
  (cond ((not (tagged-value? val)) #f)
        (else (eq? (car val) tag))))

(define (get-tag val)
  (cond (not (tagged-value? val) (error "not a tagged value " val))
        (else (car val))))

(define (tagged-value? val)
  (and (pair? val) (symbol? (car val))))

(define (unwrap-tag val)
  (cond ((not (tagged-value? val)) (error "not a tagged value " val))
        (else (cdr val))))

;end tagging

;complex number dispatch

(define (make-from-real-imag x y)
  (add-tag 'rect (make-from-real-imag-rect x y)))

(define (make-from-mag-ang r a)
  (add-tag 'polar (make-from-mag-ang-polar r a)))

(define ops '())

(define (find-ops-for-tag tagname)
  (define (iter ops)
    (cond ((null? ops) #f)
          ((eq? (caar ops) tagname) (car ops))
          (else (iter (cdr ops)))))
  (iter ops))

(define (remove-ops-for-tag tagname ops)
  (define (iter ops)
    (cond ((null? ops) '())
          ((eq? (caar ops) tagname) (cdr ops))
          (else (cons (car ops) (iter (cdr ops))))))
  (iter ops))

(define (install-operation tag-name op-name procedure)
  (let ((ops-for-tag (find-ops-for-tag tag-name)))
    (if ops-for-tag
        (set! ops (cons (append ops-for-tag (list (cons op-name procedure))) (remove-ops-for-tag tag-name ops)))
        (begin
          (set! ops (cons (cons tag-name '()) ops))
          (install-operation tag-name op-name procedure)))))
  
(define (apply-operation op-name arg)
  #f)

(define (imag-part z)
  (apply-operation 'imag-part z))

(define (real-part z)
  (cond ((is-tag? 'rect z) (real-part-rect (unwrap-tag z)))
        ((is-tag? 'polar z) (real-part-polar (unwrap-tag z)))
        (else (error "unknown tag " (get-tag z)))))

(define (magnitude z)
  (cond ((is-tag? 'rect z) (magnitude-rect (unwrap-tag z)))
        ((is-tag? 'polar z) (magnitude-polar (unwrap-tag z)))
        (else (error "unknown tag " (get-tag z)))))

(define (angle z)
  (cond ((is-tag? 'rect z) (angle-rect (unwrap-tag z)))
        ((is-tag? 'polar z) (angle-polar (unwrap-tag z)))
        (else (error "unknown tag " (get-tag z)))))

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