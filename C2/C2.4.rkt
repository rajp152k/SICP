#lang racket

;2.73
;data directed dispatch for symbolic diff

(define *op-table* (make-hash))

(define (put op proc)
  (hash-set! *op-table* (list op) proc))

(define (get op)
  (hash-ref *op-table* (list op) '()))

(define (operator expr) (car expr))
(define (operands expr) (cdr expr))

(define (variable? e)
  (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (deriv expr var)
  (cond [ (number? expr) 0 ]
        [ (variable? expr) (if (same-variable? expr var) 1 0) ]
        [ #t ((get (operator expr)) expr
                                    var)]))

(define (install-deriv-sum)
  (define (make-sum . args)
    (let ((args-redux (filter (λ (x) (not (eq? x 0) )) args)))
      (cond [(null? args-redux) 0]
            [(null? (cdr args-redux)) (car args-redux)]
            [#t (append '(+) args-redux)])))
  (define (addend s) (cadr s))
  (define (augend s)
    (apply make-sum (cddr s)))
  
  (put '+ (λ (expr var)
            (make-sum (deriv (addend expr) var)
                      (deriv (augend expr) var)))))

(define (install-deriv-product)
  (define (make-sum . args)
    (let ((args-redux (filter (λ (x) (not (eq? x 0) )) args)))
      (cond [(null? args-redux) 0]
            [(null? (cdr args-redux)) (car args-redux)]
            [#t (append '(+) args-redux)])))
  (define (make-prod . args)
    (let ((zero-arg (foldl (λ (x y) (or x y)) #f (map (λ (x) (eq? x 0)) args)))
          (args-redux (filter (λ (x) (not (eq? x 1))) args)))
      (cond [zero-arg 0]
            [(null? (cdr args-redux)) (car args-redux)]
            [#t (append '(*) args-redux)])))
  (define (multiplier p) (cadr p))
  (define (multiplicand p)
    (apply make-prod (cddr p)))

  (put '* (λ (expr var)
            (make-prod (make-sum (make-prod (multiplier expr) (deriv (multiplicand expr) var))
                                 (make-prod (deriv (multiplier expr)  var) (multiplicand expr)))))))

(define (install-deriv-exp)
  (define (make-sum . args)
    (let ((args-redux (filter (λ (x) (not (eq? x 0) )) args)))
      (cond [(null? args-redux) 0]
            [(null? (cdr args-redux)) (car args-redux)]
            [#t (append '(+) args-redux)])))
  (define (make-prod . args)
    (let ((zero-arg (foldl (λ (x y) (or x y)) #f (map (λ (x) (eq? x 0)) args)))
          (args-redux (filter (λ (x) (not (eq? x 1))) args)))
      (cond [zero-arg 0]
            [(null? (cdr args-redux)) (car args-redux)]
            [#t (append '(*) args-redux)])))
  (define (base e) (cadr e))
  (define (exponent e) (caddr e))
  (define (make-exponent b e)
    (cond [(eq? e 0) 1]
          [(eq? e 1) b]
          [#t (list '** b e)]))
  (put '** (λ (expr var)
             (make-prod (make-prod (exponent expr)
                                   (make-exponent (base expr) (make-sum (exponent expr) -1)))))))


; deriv-sum has to be installed in deriv product 
; dependency management thread possible
(install-deriv-sum)
(install-deriv-product)
(install-deriv-exp)


;2.75

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond [(eq? op 'real-part) (* (cos ang) mag)]
          [(eq? op 'imag-part) (* (sin ang) mag)]
          [(eq? op 'magnitude) mag]
          [(eq? op 'angle) ang]
          [#t (error "Unknown op -- MAKE-FROM-MAG-ANG" op)]))
  dispatch)
