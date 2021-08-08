#lang racket 

(define (in-list? ele lis)
  (foldl (λ (a b) (or a b)) #f (map (λ (x) (equal? ele x)) lis)))

;3.7
(define (make-account balance password)
  (let ((inc-count 0)
        (pwds (list password)))
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
      (if (in-list? p pwds)
          (cond [(eq? m 'withdraw) withdraw]
                [(eq? m 'deposit) deposit]
                [(eq? m 'add-pwd) (λ (pwd)
                                    (set! pwds (cons pwd pwds))) ]
                [#t (error "INVALID DEMAND REQUESTED -- make-account"
                           m)])
          (begin
            (set! inc-count (+ inc-count 1))
            (process-incorrect-password)
            (λ (x) "cops haven't been alerted, keep trying"))))
    dispatch))

(define (make-joint og-acc og-pwd new-pwd)
  ((og-acc og-pwd 'add-pwd) new-pwd)
  og-acc)


;3.8

(define f
  (let ((first-call #t))
    (λ (x)
      (if first-call
          (begin
            (set! first-call #f)
            x)
          0))))
