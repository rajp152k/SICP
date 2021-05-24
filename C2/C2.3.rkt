#lang racket

;2.54
(define (my-equal? a b)
  (cond [(and (null? a)
              (null? b)) #t]
        [(or (null? a)
             (null? b)) #f]
        [(and (pair? (car a))
              (pair? (car b))) (and (my-equal? (car a) (car b))
                                    (my-equal? (cdr a) (cdr b)))]
        [(and (not (pair? (car a)))
              (not (pair? (car b)))) (and (eq? (car a) (car b))
                                          (my-equal? (cdr a) (cdr b)))]
        [#t #f]))

;2.56-2.57

(define (variable? e)
  (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;(define (make-sum a1 a2)
;  (cond [(eq? a1 0) ]
;        [(eq? a2 0) a1]
;        [(and (number? a1) (number? a2)) (+ a1 a2)]
;        [#t (list '+ a1 a2)]))

(define (make-sum . args)
  (let ((args-redux (filter (λ (x) (not (eq? x 0) )) args)))
    (cond [(null? args-redux) 0]
          [(null? (cdr args-redux)) (car args-redux)]
          [#t (append '(+) args-redux)])))

;(define (make-prod a1 a2)
;  (cond [(or (eq? a1 0) (eq? a2 0)) 0]
;        [(eq? a1 1) a2]
;        [(eq? a2 1) a1]
;        [(and (number? a1) (number? a2)) (* a1 a2)]
;        [#t (list '* a1 a2)]))

(define (make-prod . args)
  (let ((zero-arg (foldl (λ (x y) (or x y)) #f (map (λ (x) (eq? x 0)) args)))
        (args-redux (filter (λ (x) (not (eq? x 1))) args)))
    (cond [zero-arg 0]
          [(null? (cdr args-redux)) (car args-redux)]
          [#t (append '(*) args-redux)])))

(define (sum? s)
  (and (pair? s) (eq? (car s) '+)))
(define (addend s) (cadr s))
;(define (augend s) (caddr s))
(define (augend s)
  (apply make-sum (cddr s)))

(define (product? p)
  (and (pair? p) (eq? (car p) '*)))
(define (multiplier p) (cadr p))
;(define (multiplicand p) (caddr p))
(define (multiplicand p)
  (apply make-prod (cddr p)))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponent b e)
  (cond [(eq? e 0) 1]
         [(eq? e 1) b]
         [#t (list '** b e)]))

(define (deriv expr var)
  (cond [(number? expr) 0]
        [ (variable? expr) (if (same-variable? expr var)
                              1 0)]
        [(sum? expr) (make-sum (deriv (addend expr) var)
                               (deriv (augend expr) var))]
        [(product? expr) (make-sum (make-prod (multiplier expr) (deriv (multiplicand expr) var))
                                   (make-prod (deriv (multiplier expr) var) (multiplicand expr)))]
        [(exponentiation? expr) (make-prod (make-prod (exponent expr)
                                                      (make-exponent (base expr) (make-sum (exponent expr) -1)))
                                           (deriv (base expr) var))]
        [#t (error "invalid expression encountered" expr)]))