;1.3

(defn square [x] (* x x))

(defn sum-list [l]
  (nth  (accumulate l +) (dec (len l))))

(defn sum-of-squares [l]
  (sum-list (list (map square l))))

(defn max-2-sum-squares [x y z]
  (setv minimum (square (min [x y z])))
  (setv all-sum (sum-of-squares [x y z]))
  (- all-sum minimum))

;1.7


(defn average [l]
  (/ (sum-list l) (len l)))

(defn relative-good-enough? [guess x]
  (setv new-guess (improve guess x))
  (setv diff (abs (- new-guess guess)))
  (< diff (* 0.00001 guess)))

(defn improve [guess x]
  (average [guess (/ x guess)]))

(defn relative-iterative-square-root [guess x]
  #[[returns a guess for the square root of the supplied number]]
  (if (relative-good-enough? guess x)
      guess
      (relative-iterative-square-root (improve guess x) x)))

(defn relative-sqrt [x]
  (relative-iterative-square-root 1.0 x))

; 1.8

(defn product-list [l]
  (nth (accumulate l *) (dec (len l))))

(defn cube [x] (product-list [x x x]))

(defn good-enough-cube-root? [guess x]
  (< (abs (- (cube guess) x)) 0.0001)) 

(defn improve-cube-root [guess x]
  (/ (+ (/ x 
          (square guess)) 
        (* 2 guess))
      3))

(defn iterative-cube-root [guess x]
  (if (good-enough-cube-root? guess x)
      guess
      (iterative-cube-root (improve-cube-root guess x) x)))

(defn cube-root [x]
  (iterative-cube-root 1.0 x))

;1.11

(defn f-1-11 [n]
  (defn 3-tuple [x y z] (, x y z))
  (defn get-next [cache]
    (+ (last cache) (* 2 (second cache)) (* 3 (first cache))))
  (defn iter-f-1-11 [cache m]
    (cond [(< n 3) n]
          [(= m n) (get-next cache)]
          [(< m n) (iter-f-1-11 (, (second cache)
                                   (last cache)
                                   (get-next cache))
                                (inc m))]))
  (iter-f-1-11 (, 0 1 2) 3))

;1.12

(defn C [n r]
  #[[ n choose r]]
  (cond [(or (< n 0)
            (< n r)) 0]
        [(< r 0) (print "invalid r")]
        [(= r 0) 1]
        [True (+  (C (dec n) r)
                  (C (dec n) (dec r)))])) 

; 1.16 : Fast exp

(defn exp [b n] 
  (defn fast-valid? [counter n]
    (<= (* 2 counter) n))
  (defn sqr [x] (* x x))
  (defn iter-exp [result counter]
    (cond [(= counter n) result]
          [(fast-valid? counter n) 
           (iter-exp (sqr result) (* counter 2))]
          [True (iter-exp (* result b) (inc counter))]))
  (cond [(= n 0) 1] 
        [True (iter-exp b 1)]))

; 1.22 : perfcounter :as runtime

; basic runtime test frame
; see prime? in utils

(import [utils [*]])

(defn timed-prime-test [n]
  (print "\n")
  (print f"{n}")
  (start-prime-test n (runtime)))

(defn start-prime-test [n start-time]
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(defn report-prime [elapsed-time]
  (print "***")
  (print f"{elapsed-time}"))

(defn search-for-primes [k n]
  #[[outputs first k primes >= n]]
  (defn testnext [primes left num]
    (cond [(zero? left) primes] 
          [(prime? num) (testnext (+ primes [num]) (dec left) (+ num 2))]
          [True (testnext primes left (inc num))]))
  (print f"found {k} primes >= {n} as : {(testnext [] k n)}"))


; 1.24
(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (random.randint 1 (dec n))))

(defn fast-prime? [n times]
  (cond [(zero? times) True] 
        [(fermat-test n) (fast-prime? n (dec times))]
        [True False]))

(defn timed-prime-test [n]
  (print "\n")
  (print f"{n}")
  (start-prime-test n (runtime)))

(defn start-prime-test [n start-time]
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(defn report-prime [elapsed-time]
  (print "***")
  (print f"{elapsed-time}"))
