#lang racket
(require sicp-pict)

;prereqs
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((small-right (right-split painter (- n 1))))
        (beside painter (below small-right small-right)))))

;2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((small-up (up-split painter (- n 1))))
        (below painter (beside small-up small-up)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;2.45

(define (split root-split child-split)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((child (splitter painter (- n 1))))
          (root-split painter
                      (child-split child child)))))
  splitter)

;2.46
;(define (make-vect x y)
;  (cons x y))
;(define (xcor-vect v) (car v))
;(define (ycor-vect v) (cdr v))
;(define (add-vects v1 v2)
;  (make-vect (+ (xcor-vect v1)
;                (xcor-vect v2))
;             (+ (ycor-vect v1)
;                (ycor-vect v2))))
;(define (sub-vects v1 v2)
;  (make-vect (- (xcor-vect v1)
;                (xcor-vect v2))
;             (- (ycor-vect v1)
;                (ycor-vect v2))))
;(define (scale-vect scale v)
;  (make-vect (* scale
;                (xcor-vect v))
;             (* scale
;                (ycor-vect v))))

;2.47 : bunch of cars and cdrs

;2.48
(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

;prereqs
(define (draw-line v1 v2)
  "I don't do shit"
  '())
(define (segments->painter segments)
  (λ (frame)
    (let ((mapper (frame-coord-map frame)))
      (for-each
       (λ (segment)
         (draw-line
          (mapper (start-segment segment))
          (mapper (end-segment segment))))
       segments))))

;2.49
;a
(define outline
  (let ((s make-segment)
        (v make-vect))
    (segments->painter (list (s (v 0 0) (v 0 1))
                             (s (v 0 1) (v 1 1))
                             (s (v 1 1) (v 1 0))
                             (s (v 1 0) (v 0 0))))))

;2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vector 1.0 0.0)
                     (make-vector 0.0 0.0)
                     (make-vector 1.0 1.0)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vector 1.0 1.0)
                     (make-vector 1.0 0.0)
                     (make-vector 0.0 1.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vector 1.0 0.0)
                     (make-vector 1.0 1.0)
                     (make-vector 0.0 0.0)))

;2.51
(define (below painter1 painter2)
  (λ (frame)
    (((transform-painter painter1
                         (make-vector 0.0 0.0)
                         (make-vector 1.0 0.0)
                         (make-vector 0.0 0.5)) frame)
     ((transform-painter painter2
                         (make-vector 0.0 0.5)
                         (make-vector 1.0 0.5)
                         (make-vector 0.0 1.0))))))

(define (bes-below painter1 painter2)
  (rotate90 (besides (rotate270 painter2)
                     (rotate270 painter1))))


