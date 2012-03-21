;; Arithmetic problems

(defn is-prime
  "P31 (**) Determine whether a given integer number is prime."
  { :_test '[(true? (is-prime 7))
             (true? (is-prime 101))
             (true? (is-prime 2))
             (false? (is-prime 4))
             (false? (is-prime 51))
             (false? (is-prime 1))] }

  [number]
  (condp = number
    1 false
    2 true
    (let [divisors (range 2 (max 3 (bit-shift-right number 1)))]
      (not (some zero? (map #(rem number %) divisors))))))

(defn gcd
  "P32 (**) Determine the greatest common divisor of two positive integer numbers."
  { :_test '[(= (gcd 36 63) 9)
             (= (gcd 0 1) 1)
             (= (gcd 13 101) 1)] }

  [a b]
  ((fn ord-gcd [big small]
     (if (zero? small)
       big
       (recur small (rem big small))))
   (max a b) (min a b)))

