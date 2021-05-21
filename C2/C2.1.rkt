#lang racket ;prereqs 
(define (numer rat) (car rat))
(define (denom rat) (cdr rat))
(define (print-rat rat)
  (newline)
  (display (numer rat))
  (display " / ")
  (display (denom rat)))

;2.1
(define (make-rat n d)
  (let ((g (gcd n d))
        (n_ ((if (< d 0) (- n) n))))
    (cons (/ n_ g) (/ (abs d) g))))


;2.2

(define (make-coord x y)
  (cons x y))

(define (x-pt p)
  (car p))

(define (y-pt p)
  (cdr p))

(define (make-seg p1 p2)
  (cons p1 p2))

(define (start-p s)
  (car s))

(define (end-p s)
  (cdr s))

(define (average a b)
  (/ (+ a b)
     2))

(define (print-pt p)
  (newline)
  (display "( ")
  (display (x-pt p))
  (display " , ")
  (display (y-pt p))
  (display " )"))

(define (mid-pt seg)
  (let* ((p1 (start-p seg))
         (p2 (end-p seg)))
    (make-coord
     (average (x-pt p1)
              (x-pt p2))
     (average (y-pt p1)
              (y-pt p2)))))

;2.3
(define (make-rect p1 p2)
  "returns a rectangle given the top left and bottom right points"
  (cons p1 p2))

(define (top-left rect)
  (car rect))

(define (bottom-right rect)
  (cdr rect))

;(define (make-rect p l b)
;  "make a rectangle given the top left point with length and breadth"
;  (cons p (cons l b)))
;
;(define (top-left rect)
;  (car rect))
;
;(define (bottom-right rect)
;  (make-coord
;   (+ (x-pt (top-left rect))
;      (car (cdr rect)))
;   (+ (y-pt (top-left rect))
;      (cdr (cdr rect)))))


(define (xdiff p1 p2)
  (abs (- (x-pt p1)
          (x-pt p2))))

(define (ydiff p1 p2)
  (abs (- (y-pt p1)
          (y-pt p2))))

(define (perimeter rect)
  (let ((p1 (top-left rect))
        (p2 (bottom-right rect)))
    (* 2
       (+ (xdiff p1 p2)
          (ydiff p1 p2)))))

(define (area rect)
  (let ((p1 (top-left rect))
        (p2 (bottom-right rect)))
    (* (xdiff p1 p2)
       (ydiff p1 p2))))

;2.4

(define (cons-proc x y)
  (λ (m) (m x y)))

(define (car-proc z)
  (z (λ (p q) p)))

(define (cdr-proc z)
  (z (λ (p q) q)))


;2.5

(define (cons-num x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (extract-exp base z)
  (define (extract-exp-iter num res)
    (if (divides? base num)
        (extract-exp-iter (/ num base) (+ res 1))
        res))
  (extract-exp-iter z 0))

(define (car-num z)
  (extract-exp 2 z))

(define (cdr-num z)
  (extract-exp 3 z))

;2.6

(define one
  (λ (f)
    (λ (x)
      (f x))))

(define two
  (λ (f)
    (λ (x)
      (f (f x)))))

(define (church-add m n)
  (λ (f)
    (λ (x)
      ((m f) ((n f) x)))))

(define (church-mul m n)
  (λ (f)
    (λ (x)
      ((m (n f)) x))))

(define (translate-to-norm church)
  "A necessity, sanity is ..."
  ((church (λ (x) (+ x 1))) 0))

;(translate-to-norm two)
;(translate-to-norm (church-add one two))
;(translate-to-norm (church-mul (church-add one two) two))

;2.7
(define (make-interval a b)
  (cons a b))

(define (ub i)
  "upper bound"
  (cdr i))

(define (lb i)
  "lower bound"
  (car i))

;2.8

(define (sub-interval i1 i2)
  (make-interval (- (lb i1)
                    (ub i2))
                 (- (ub i1)
                    (lb i2))))

;prereqs
(define (mul-interval a b)
  (let ((p1 (* (lb a) (lb b)))
        (p2 (* (lb a) (ub b)))
        (p3 (* (ub a) (lb b)))
        (p4 (* (ub a) (ub b))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;2.10

(define (spans-across? num interval)
  (and ((<= (lb interval) num)
        (>= (ub interval) num))))

(define (spans-zero? i) (spans-across? 0 i))

(define (div-interval a b)
  (if (spans-zero? b)
      "Zero division error"
      (mul-interval
       a
       (let ((lower (min (/ 1 (lb b)) (/ 1 (ub b))))
             (upper (max (/ 1 (lb b)) (/ 1 (ub b)))))
         (make-interval lower upper)))))


;2.11

; 3 possibilites for one interval: 9 for two
; when both span zero : more than two multiplications
; otherwise two multiplications with certainty


;2.12


(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-interval (- c w)
                   (+ c w))))

(define (center i)
  (/ (+ (lb i)
        (ub i))
     2))

(define (percent i)
  (* 50 
     (/ (- (ub i)
           (lb i))
        (center i))))

;2.13
; addition of individual tolerances.
; expand and ignore small terms
