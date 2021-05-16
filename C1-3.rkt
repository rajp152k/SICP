#lang racket

;1.29 + 1.30

(define (integrate a fn b n)
  "approximate int(fn a to b) using \"Simpson's rule"
  (if (odd? n) (set n (+ n 1)) #t)
  (define h (/ (- b a ) n))
  (define (factor k)
    (cond [(or (= k 1) (= k n)) 1]
          [(odd? k) 4]
          [#t 2]))
  (define (term k)
    (fn (+ a (* k h))))
  (define (acc-res i res) ; iterative acc(1.30)
    (if (> i n)
        res
        (acc-res (+ i 1)
                 (+ res
                    (* (factor i)
                       (term i))))))
  (* (/ h 3) (acc-res 0 0)))

;1.31

(define (product fn a step b)
  (define (acc-prod curr res)
    (if (> curr b)
        res
        (acc-prod (+ curr step)
                  (* res (fn curr)))))
  (acc-prod a 1))

(define (factorial n)
  (product identity 1 1 n))

(define (square x) (* x x))
(define (pi  n)
  (* (/ (* 2
           (+ (* 2 n) 2)
           (square (product identity 4 2 (* 2 n))))
        (square (product identity 3 2 (+ 1 (* 2 n)))))
     4.0))
