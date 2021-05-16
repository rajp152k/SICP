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

;1.32
(define (accumulate combiner initial fn a next b)
  (define (acc-res curr res)
    (if (> curr b)
        res
        (acc-res (next curr)
                 (combiner (fn curr) res))))
  (acc-res a initial))

(define (acc-product fn a step b)
  (accumulate (lambda (x y) (* x y))
              1
              fn
              a
              (lambda (x) (+ x step))
              b))

(define (acc-sum fn a step b)
  (accumulate (lambda (x y) (+ x y))
              0
              fn
              a
              (lambda (x) (+ x step))
              (b)))

;1.33

(define (filtered-accumulate combiner fltr initial fn a next b)
  (define (filter-compute-combine curr res)
    (if (fltr curr)
        (combiner (fn curr) res)
        res))
  (define (acc-res curr res)
    (if (> curr b)
        res
        (acc-res (next curr)
                 (filter-compute-combine curr res))))
  (acc-res a initial))
