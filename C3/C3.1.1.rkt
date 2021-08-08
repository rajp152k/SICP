#lang racket

;3.1
(define (make-accumulator initial)
  (λ (inc)
    (set! initial (+ initial inc))
    initial))

;3.2
(define (make-monitored f)
  (let ((ctr 0))
    (define (num-calls)
      ctr)
    (define (reset-num-calls)
      (set! ctr 0))
    (define (dispatch m)
      (cond [(eq? m 'how-many-calls?) (num-calls)]
            [(eq? m 'reset-count) (reset-num-calls)]
            [#t (begin
                  (set! ctr (+ ctr 1))
                  (f m))]))
    dispatch))

;testing proc
(define m-inc (make-monitored (λ (arg) (+ arg 1))))

;3.3, 3.4

(define (make-account balance password)
  (let ((inc-count 0))
    (define (call-the-cops) 1729)
    (define (process-incorrect-password)
      (if (>= inc-count 7)
          (call-the-cops)
          (error "incorrect password : request un-executed")))
    (define (withdraw amount)
      (if (<= amount balance)
          (begin
            (set! balance (- balance amount))
            balance)
          (begin
            (display "insufficient balance: ")
            (display balance)
            balance)))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch p m)
      (if (eq? p password)
          (cond [(eq? m 'withdraw) withdraw]
                [(eq? m 'deposit) deposit]
                [#t (error "INVALID DEMAND REQUESTED -- make-account"
                           m)])
          (begin
            (set! inc-count (+ inc-count 1))
            (process-incorrect-password)
            (λ (x) "cops haven't been alerted, keep trying"))))
    dispatch))

;testing proc
(define acc (make-account 100 'asdf))
