#lang racket

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




(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-rect-polar z)
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

