
#lang racket

(define (make-wire)
  (define value 0)
  (define actions '())
  (define (set-signal! new-value) 
    (if (eq? new-value value) 'same
        (begin
          (set! value new-value)
          (for-each (lambda (action) (action)) actions))))
  (define (add-action! action)
    (set! actions (cons action actions))
    (action)
    )
  (define (get-value)
    value)
  (lambda (msg)
    (cond
      ((eq? msg 'get-value) get-value)
      ((eq? msg 'set-signal!) set-signal!)
      ((eq? msg 'add-action!) add-action!)
      (else (error "unknown message" msg))
    )))

(define (after-delay delay procedure)
  ;(display "waiting for ")
  ;(display delay)
  ;(newline)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  procedure
                  the-agenda))

(define (or-gate in1 in2 out) 
  (define or-delay 5)
  (define (logical-or v1 v2)
    (if (or (eq? v1 1) (eq? v2 1)) 1 0))
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal in1) (get-signal in2))))
      (after-delay 
       or-delay
       (lambda ()
         (set-signal! out new-value)))))
  (add-action! in1 or-action-procedure)
  (add-action! in2 or-action-procedure)
  'ok)

(define (probe name wire)
  (define (print)
    (display name)
    (display " ")
    (display (current-time the-agenda))
    (display " New-value = ")
    (display (get-signal wire))
    (newline))
  (add-action! wire print))

(define (or-gate-nand in1 in2 out) 
  (define a (make-wire))
  (define aa (make-wire))
  (define b (make-wire))
  (define bb (make-wire))
  (define c (make-wire))
  (define cc out)
  (inverter a aa)
  (inverter b bb)
  (inverter c cc)
  (and-gate in1 in1 a)
  (and-gate in2 in2 b)
  (and-gate aa bb c)
  'ok)

(define (and-gate in1 in2 out) 
  (define and-delay 3)
  (define (logical-and val1 val2)
    (if (and (eq? 1 val1) (eq? 1 val2)) 1 0))
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal in1) (get-signal in2))))
      (after-delay 
       and-delay
       (lambda ()
         (set-signal! out new-value)))))
                   
  
  (add-action! in1 and-action-procedure)
  (add-action! in2 and-action-procedure)
  'ok)

(define (inverter in out)
  (define inverter-delay 2)
  (define (logical-not value) 
    (cond ((eq? value 0) 1)
          ((eq? value 1) 0)
          (else (error "unknown signal " value))))
  (define (invert-input)
    (let ((out-signal (logical-not (get-signal in))))
      (after-delay 
       inverter-delay 
       (lambda () 
         (set-signal! out out-signal)))))
  (add-action! in invert-input)
  'ok)

(define (half-adder A B S C)
  (let ((D (make-wire))
        (E (make-wire)))
    (and-gate D E S)
    (or-gate A B D)
    (and-gate B A C)
    (inverter C E)
    'ok))

(define (full-adder A B Cin SUM Cout)
  (let ((D (make-wire))
        (E (make-wire))
        (F (make-wire)))
    (half-adder A D SUM E)
    (half-adder B Cin D F)
    (or-gate E F Cout))
  'ok)

(define (get-signal wire)
  ((wire 'get-value)))

(define (set-signal! wire value)
  ((wire 'set-signal!) value)
  'ok)

(define (add-action! wire action)
  ((wire 'add-action!) action)
  'ok)


(define (ripple-carry-adder A B S C)
  (define C0 (make-wire))
  (define (thread-adder A B S Cin)
    (cond ((null? A) 'done)
          (else
           (define Cout (if (null? (cdr A)) C (make-wire)))
           (full-adder (car A) (car B) Cin (car S) Cout)
           (thread-adder (cdr A) (cdr B) (cdr S) Cout))))
  (thread-adder A B S C0))



(define (make-wires-n n)
  (if (= 0 n) '() (cons (make-wire) (make-wires-n (- n 1)))))

(define (set-wires-value! wires value)
  (cond ((null? wires) 'done)
        (else
         (set-signal! (car wires) (car value))
         (set-wires-value! (cdr wires) (cdr value)))))
  

(define (make-wires values)
  (define wires (make-wires-n (length values)))
  (set-wires-value! wires values)
  wires)

(define (display-number-from-wires wires)
  (cond ((null? wires) (newline))
        (else 
         (display (get-signal (car wires)))
         (display-number-from-wires (cdr wires)))))


(define (make-agenda)
  (define current-time 0)
  (define actions '())
  
  (define (insert-action-at-time time action actions)
    (cond 
      ((null? actions) (cons (list time action) '()))
      (else
       (let ((first-time (caar actions)))
         (if (<= first-time time)
             (cons (car actions) (insert-action-at-time time action (cdr actions)))
             (cons (list time action) actions))))))
  
  (define (add time action)
    (set! actions (insert-action-at-time time action actions))
    )
  (define (first-item)
    (define item (car actions))
    (set! current-time (car item))
    (cadr item))
  
  (define (dispatch msg)
    (cond
      ((eq? 'current-time msg) current-time)
      ((eq? 'add msg) add)
      ((eq? 'empty? msg) (null? actions))
      ((eq? 'first-item msg) (first-item))
      ((eq? 'remove-first-item! msg) (set! actions (cdr actions)))
      (else (error "unknown msg " msg)))
    )
  dispatch)

(define (add-to-agenda! time action agenda)
  ((agenda 'add) time action))
(define (current-time agenda) (agenda 'current-time))
(define (empty-agenda? agenda) (agenda 'empty?))
(define (first-agenda-item agenda) (agenda 'first-item))
(define (remove-first-agenda-item! agenda) (agenda 'remove-first-item!))

(define the-agenda (make-agenda))

(define A (make-wire))
(define B (make-wire))
(define S (make-wire))
(define C (make-wire))

(and-gate A B C)
(set-signal! A 0)
(set-signal! B 1)

(set-signal! A 1)
(set-signal! B 0)

(probe "C" C)



(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(propagate)