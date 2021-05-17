(import [time [perf_counter :as runtime]])
(import random)

(defn pdb [x]
  #[[initiate pdb wrapper]]
  #[[pass x as data using ']]
  (breakpoint)
  (eval x))

(defn timed-eval [x]
  #[[seems glitchy ,yet to refine ]]
  ; the overheads for the setv and calling eval are way larger than the actual time taken hence all times have a minimum
  (print f"timed evaluation for {x}")
  (print f"========================")
  (setv start (runtime))
  (setv x (eval x))
  (setv end (runtime))
  (print f"time elapsed : {(- end start)}")
  (print f"========================")
  (print f"output : {x}"))


(defn square [x] (* x x))

(defn sum-list [l]
  (nth  (accumulate l +) (dec (len l))))

(defn average [l]
  (/ (sum-list l) (len l)))

(defn product-list [l]
  (nth (accumulate l *) (dec (len l))))

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

(defn smallest-divisor [n]
  (divisor? n 2))

(defn divisor? [n candidate]
  (defn nxtcand [candidate]
    (if (= candidate 2) 3 (+ candidate 2)))
  (cond [(> (square candidate) n) n]
        [(zero? (% n candidate)) candidate]
        [True (divisor? n (nxtcand candidate))]))

(defn prime? [n]
  (cond [(< n 2) False]
        [(= n (smallest-divisor n)) True]
        [True False]))

; Fermat test : a^n cogruent a mod n for prime n and a<n

(defn expmod [a n m]
  #[[returns (a^n mod m)]]
  (defn rem-m  [num] (% num m))
  (cond [(zero? n) 1]
        [(even? n) 
         (rem-m (square (expmod a (/ n 2) m)))]
        [True (rem-m (* a (expmod a (dec n) m)))]))


(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (random.randint 1 (dec n))))

(defn fast-prime? [n times]
  (cond [(zero? times) True] 
        [(fermat-test n) (fast-prime? n (dec times))]
        [True False]))
