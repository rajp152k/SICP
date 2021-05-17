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

;1.35

(define (fixed-point-find fn fg thresh)
  "find the fixed point of fn(x) with first guess as fg"
  (define (close-enough? a b)
    (< (abs (- a
               b))
       thresh))
  (define (average a b)
    (/ (+ a
          b)
       2))
  (define (fixed-point-iter curr)
    (let ((next (fn curr)))
      (cond [(close-enough? next curr) curr]
            [#t (fixed-point-iter (average next curr))])))
  (fixed-point-iter fg))

;;(fixed-point-find (lambda (x) (+ 1.0 (/ 1 x))) 1 0.00001)

;1.36


(define (logged-fixed-point-find fn fg thresh)
  "find the fixed point of fn(x) with first guess as fg"
  (define (close-enough? a b)
    (< (abs (- a
               b))
       thresh))
  (define (average a b)
    (/ (+ a
          b)
       2))
  (define (fixed-point-iter curr)
    (let ((next (fn curr)))
      (cond [(close-enough? next curr) (let ()
                                         (display "found fixed point ")
                                         (print curr)
                                         (newline)
                                         curr)]
            [#t (fixed-point-iter (let ((avg (average next curr)))
                                    (display "guessing ")
                                    (print avg)
                                    (newline)
                                    avg))])))
  (fixed-point-iter fg))


;1.37

(define (cont-frac n d k)
  (define (cont-frac-sub-exp curr)
    (if (= curr k)
        (/ (n curr)
           (d curr))
        (/ (n curr)
           (+ (d curr)
              (cont-frac-sub-exp (+ curr 1))))))
  (cont-frac-sub-exp 1))

(define (cont-frac-iter n d k)
  (define (cont-frac-sub-cache curr res)
    (if (= curr 0)
        res
        (cont-frac-sub-cache (- curr 1)
                             (/ (n curr)
                                (+ (d curr)
                                   res)))))
  (cont-frac-sub-cache k 0))


;1.38

(define (cont-frac-guess-e approx-extent)
  (+ 2
     (let ((n (lambda (x) 1.0))
           (d (lambda (x)
                (cond [(zero? (remainder (+ x 1) 3)) (* 2 (quotient (+ x 1) 3))]
                      [#t 1]))))
       (cont-frac-iter n d approx-extent))))


;1.39

(define (tan-cf x k)
  "compute tan within -pi/2 to pi/2"
  (define (cont-frac-iter-neg n d k)
    (define (cont-frac-sub-cache curr res)
      (if (= curr 0)
          res
          (cont-frac-sub-cache (- curr 1)
                               (/ (n curr)
                                  (- (d curr)
                                     res)))))
    (cont-frac-sub-cache k 0.0))
  (let ((n (lambda (k) (expt x k)))
        (d (lambda (k) (- (* 2 k) 1))))
    (cont-frac-iter-neg n d k)))
