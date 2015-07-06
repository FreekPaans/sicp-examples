#lang racket


(define (for-each-except except f items)
  (for-each (lambda (item)
              (if (eq? except item) 'ignore (f item))) items))

(define (adder a b c)
  (define (inform-about-value)
    (cond ((and (has-value? a) (has-value? b)) (set-value! c (+ (get-value a) (get-value b)) dispatch))
          ((and (has-value? a) (has-value? c)) (set-value! b (- (get-value c) (get-value a)) dispatch))
          ((and (has-value? b) (has-value? c)) (set-value! a (- (get-value c) (get-value b)) dispatch))
          (else 'not-specified)))
  (define (inform-about-no-value)
    (forget-value! a dispatch)
    (forget-value! b dispatch)
    (forget-value! c dispatch)
    (inform-about-value))
  (define dispatch 
         (lambda (msg)
           (cond ((eq? msg 'inform-about-value) (inform-about-value))
                 ((eq? msg 'inform-about-no-value) (inform-about-no-value))
                 (else (error "message not understood" msg))))
         )
  (connect a dispatch)
  (connect b dispatch)
  (connect c dispatch)
  dispatch)

(define (multiplier a b c)
  (define (inform-about-value)
    (cond ((and (has-value? a) (has-value? b)) (set-value! c (* (get-value a) (get-value b)) me))
          ((and (has-value? a) (has-value? c)) (set-value! b (/ (get-value c) (get-value a)) me))
          ((and (has-value? b) (has-value? c)) (set-value! a (/ (get-value c) (get-value b)) me))))
  (define (inform-about-no-value)
    (forget-value! a me)
    (forget-value! b me)
    (forget-value! c me)
    (inform-about-value))
  (define me (lambda (msg)
               (cond ((eq? msg 'inform-about-value) (inform-about-value))
                     ((eq? msg 'inform-about-no-value) (inform-about-no-value))
                     (else (error "mesage not understood" msg)))))
  (connect a me)
  (connect b me)
  (connect c me)
  me)
  

(define (constant value x)
  (define me (lambda (msg) (error "message not understood" msg)))
  (connect x me)
  (set-value! x value me)
  me)

(define (make-connector)
  (define constraints '())
  (define value false)
  (define informant false)
  (define (connect constraint)
    (set! constraints (cons constraint constraints)))
  (define (set-value! new-value new-informant)
    (cond ((not (has-value? me))
           (set! value new-value)
           (set! informant new-informant)
           (for-each-except informant (lambda (constraint) (constraint 'inform-about-value)) constraints))
          ((not (= new-value value) (error "Contradiction" (list new-value value))))
          (else 'ignored)))
  (define (forget-value! retractor)
    (if (not (eq? retractor informant)) 'ignore
        (begin
          (set! informant false)
          (set! value false)
          (for-each-except retractor inform-about-no-value constraints))))
           
  (define me (lambda (msg)
    (cond ((eq? msg 'set-value!) set-value!)
          ((eq? msg 'connect) connect)
          ((eq? msg 'get-value) value)
          ((eq? msg 'has-value?) (if informant true false))
          ((eq? msg 'forget-value!) forget-value!)
          (else (error "message not understood " msg)))))
  me)

(define (probe name connector)
  (connect connector
           (lambda (msg)
             (cond ((eq? msg 'inform-about-value)
                    (newline)
                    (display name)
                    (display " ")
                    (display (get-value connector)))))))
                       


(define (set-value! connector value informant)
  ((connector 'set-value!) value informant))
(define (forget-value! connector retractor)
  ((connector 'forget-value!) retractor))
(define (get-value connector)
  (connector 'get-value))
(define (has-value? connector)
  (connector 'has-value?))
(define (connect connector constraint)
  ((connector 'connect) constraint))

(define (inform-about-no-value constraint)
  (constraint 'inform-about-no-value))

(define (celsius-fahrenheit C F)
  (let ((w (make-connector))
        (u (make-connector))
        (x (make-connector))
        (v (make-connector))
        (y (make-connector)))
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    (multiplier w C u)
    (multiplier x v u)
    (adder v y F)
    'ok))


(define C (make-connector))
(define F (make-connector))

(celsius-fahrenheit C F)

(probe "Celsius" C)
(probe "Fahrenheit" F)

(set-value! C 100 'user)
(forget-value! C 'user)
(set-value! F 32 'user)


;(define a (make-connector))
;(define b (make-connector))
;(define c (make-connector))

;(probe "a" a)
;(probe "b" b)
;(probe "c" c)

;(multiplier a b c)

;(set-value! a 100 'user)
;(set-value! b 100 'user)
;(forget-value! b 'user)
;(set-value! c 150 'user)

;(set-value! C 100 'user)


