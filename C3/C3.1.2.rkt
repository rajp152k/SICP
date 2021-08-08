#lang racket

;3.5
(random-seed 1729)

(define (make-point x y)
  (λ m  
    (cond [(equal? m '()) (begin
                            (display "x: ")
                            (display x)
                            (newline)
                            (display "y: ")
                            (display y)
                            (newline))]
          [(equal? m '(x)) x]
          [(equal? m '(y)) y]
          [#t (error "invalid request")])))

(define (square x) (* x x))
(define (sqr-euc a b) (square (- a b)))

(define (make-circle point r)
  (λ m  
    (cond [(equal? m '()) (begin
                            (display "center: ")
                            (newline)
                            (point)
                            (newline)
                            (display "radius: ")
                            (display r)
                            (newline))]
          [(equal? m '(center)) point]
          [(equal? m '(radius)) r]
          [(equal? m '(p-in?)) (λ (p)
                                 (<= (+ (sqr-euc (p 'x) (point 'x))
                                        (sqr-euc (p 'y) (point 'y)))
                                     (square r)))]
          [(equal? m '(top-left-p)) (make-point (- (point 'x) r)
                                                (+ (point 'y) r))]
          [(equal? m '(bottom-right-p)) (make-point (+ (point 'x) r)
                                                    (- (point 'y) r))]
          [(equal? m '(rect-area)) (* 4 (square r))]
          [(equal? m '(area)) (* pi (square r))]
          [#t (error "invalid request")])))

;test procs

(define c (make-point 5 7))
(define r 3)
(define circ (make-circle c r))
;((circle 'p-in?) (circle 'center)) -> #t
;((circle 'p-in?) (circle 'top-left-p)) -> #f


(define (monte-carlo experiment num-trials)
  (define (monte-carlo-iter trials-left trials-successful)
    (cond [(= trials-left 0) trials-successful]
          [#t (if (experiment)
                  (monte-carlo-iter (- trials-left 1) (+ trials-successful 1))
                  (monte-carlo-iter (- trials-left 1) trials-successful))]))
  (/ (monte-carlo-iter num-trials 0)
     num-trials))

(define (estimate-circle-area circle num-trials)
  (define (gen-rand-point lo_x hi_x lo_y hi_y)
    (define (random-range lo hi)
      (+ lo
         (* (- hi lo)
            (random))))
    (let ((xr (random-range lo_x hi_x))
          (yr (random-range lo_y hi_y)))
      (make-point xr yr)))
  (define (circ-rand-check-exp)
    (let ((lo_x ((circle 'top-left-p) 'x))
          (hi_x ((circle 'bottom-right-p) 'x))
          (lo_y ((circle 'bottom-right-p) 'y))
          (hi_y ((circle 'top-left-p) 'y)))
      ((circle 'p-in?) (gen-rand-point lo_x hi_x
                                       lo_y hi_y))))
  (exact->inexact (* (circle 'rect-area)
                     (monte-carlo circ-rand-check-exp num-trials))))
