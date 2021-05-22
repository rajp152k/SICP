#lang racket

;2.17
(define (last-pair lis)
  (if (null? (cdr lis))
      lis
      (last-pair (cdr lis))))

;prereqs
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

;2.18
(define (reverse lis)
  (if (null? lis)
      lis
      (append (reverse (cdr lis)) (list(car lis)))))

;2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;(cc 100 us-coins)


(define (cc amount coin-values)
  (define no-more? null?)
  (define except-first-denomination cdr)
  (define first-denomination car)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;2.20
(define (filter p lis)
  (define (filter-cache curr res)
    (if (null? curr)
        res
        (if (p (car curr))
            (filter-cache (cdr curr)
                          (cons (car curr) res))
            (filter-cache (cdr curr)
                          res))))
  (reverse (filter-cache lis '())))

(define (same-parity x . y)
  (define p (if (even? x)
                even?
                odd?))
  (cons x
        (filter p y)))

;2.21

(define (square-list lis)
  (if (null? lis)
      '()
      (cons (* (car lis) (car lis))
            (square-list (cdr lis)))))

(define (square-list2 lis)
  (map (λ (x) (* x x)) lis))


;2.22
(define (for-each fn lis)
  (if (null? lis)
      #t
      (and (or (fn (car lis)) #t)
           (for-each fn (cdr lis)))))

;2.27
(define (deep-reverse lis)
  (cond [(null? lis) '()]
        [(not (pair? lis)) lis]
        [#t (append (deep-reverse (cdr lis))
                    (list (deep-reverse (car lis))))]))

;2.28
(define (fringe tree)
  (cond [(null? tree) '()]
        [(not (pair? tree))  (list tree)]
        [(append (fringe (car tree))
                 (fringe (cdr tree)))]))

;prereqs

(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

;2.29

;a
(define (left-branch m) (car m))
(define (right-branch m) (cadr m))

(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))

;b
(define (total-weight m)
  (cond [(null? m) 0]
        [(not (pair? m)) m]
        [#t (+ (total-weight (branch-structure (left-branch m)))
               (total-weight (branch-structure (right-branch m))))]))

;c
(define (balanced? m)
  (let ((lb (left-branch m))
        (rb (right-branch m))
        (bs branch-structure)
        (bl branch-length)
        (tw total-weight))
    (cond [(not (pair? m)) #t]
          [#t (and (balanced? (bs (lb)))
                   (balanced? (bs (rb)))
                   (= (* (tw (bs lb))
                         (bl lb))
                      (* (tw (bs rb))
                         (bl rb))))])))

;d
;only change the two cadrs in part a to cdrs

;2.30

(define (square-tree tree)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (* tree tree)]
        [#t (cons (square-tree (car tree))
                  (square-tree (cdr tree)))]))

(define (square-tree-map tree)
  (map (λ (subtree)
         (if (pair? subtree)
             (square-tree-map subtree)
             (* subtree subtree)))
       tree))

;2.31

(define (tree-map fn tree)
  (map (λ (subtree)
         (if (pair? subtree)
             (tree-map fn subtree)
             (fn subtree)))
       tree))

(define (square-tree-map-2 tree)
  (tree-map (λ (x) (* x x)) tree))

;2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (λ (l) (cons (car s) l))
                          rest)))))
;; collating two types of subsets at a time : with and without the head

;prereqs

(define accumulate foldr)

;2.33


(define (acc-map fn seq)
  (accumulate (λ (x y)
                (cons (fn x)
                      y))
              '()
              seq))

(define (acc-append l1 l2)
  (accumulate cons l2 l1))

(define (acc-length lis)
  (accumulate (λ (in acc) (+ acc 1))
              0
              lis))

;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (λ (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))

;2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (λ (subtree)
                     (cond [(null? subtree) 0]
                           [(not (pair? subtree)) 1]
                           [#t (count-leaves subtree)]))
                   t)))


;2.36
(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op initial
                        (map (λ (lis) (car lis)) seqs))
            (accumulate-n op initial
                          (map (λ (lis) (cdr lis)) seqs)))))

;2.37
(define (dot-product u v)
  (accumulate + 0 (map * u v)))

(define (matrix-*-vector m v)
  (map (λ (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (λ (vect) (matrix-*-vector cols vect)) m)))

;2.39
(define (foldl-reverse seq)
  (foldl (λ (x y) (cons x y)) '() seq))

(define (foldr-reverse seq)
  (foldr (λ (x y) (append y (list x))) '() seq))
