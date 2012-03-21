;; Here I'm attempting to collect a set of routines that have to do with
;; combinations and permutations.


;; ** CNM ***
;; Count the number of ways to extract m elements from n total

(defn cnm [n m]
  (let [fact (fn [x] (reduce * (range 1 (inc x))))]
    (/ (fact n) (* (fact m) (fact (- n m))))))

;; *** SIEVE ***
;; Takes a sequence and a list of ones and zeros and returns a new sequence
;; with those elements from lst for which corresponding elements in sieve are
;; non-zeros.

(defn sieve-1
  [lst sieve]
  (map (fn [[_ e]] e)                                  ; unboxing
       (filter #(not (zero? (first %)))                ; sieving
               (partition 2 (interleave sieve lst))))) ; boxing

(defn sieve-2
  [lst sieve]
  (reduce (fn [acc [i e]] (if (pos? i) (conj acc e) acc))
          []
          (partition 2 (interleave sieve lst))))

(defn sieve-3
  [lst sieve]
  (filter (comp not nil?)
          (map (fn [i e] (if (pos? i) e nil))
               sieve
               lst)))

(def sieve sieve-3)


;; *** GEN-SIEVES ***
;; Takes the total count of elements (n) and a number of elements to select (m)
;; and generates a list of all sieves for this combination.

(defn gen-sieves [n m]
  (def results [])
  ((fn nesty [init prev-index n k acc]
    (let [upper-bound (+ init (- n k -1))]
      (doseq [i (range prev-index upper-bound)]
        (if (= upper-bound n)
          (def results (conj results (assoc acc i 1)))
          (nesty (inc init) (inc i) n k (assoc acc i 1))))))
     0 0 n m (into [] (repeat n 0)))
  results)

;; *** SELECT ***
;; Select all possible combinations of m elements from lst.

(defn select [m lst]
  (let [sieves (gen-sieves (count lst) m)]
    (for [s sieves] (sieve-1 lst s))))


;; Solve a few of the 99 Lisp problems using the functions defined above

(defn rnd-permu
  "P25 (*) Generate a random permutation of the elements of a list."
  { :_test '(= (count (rnd-permu '(a b c d e f))) 6) }

  [lst]
  (let [nelems (count lst)]
    (loop [result (vec lst), cnt nelems]
      (if (zero? cnt)
        result
        (let [index (rand-int cnt),
              tail-index (dec cnt),
              elem (nth result index),
              tail-elem (nth result tail-index)
              new-result (assoc (assoc result tail-index elem) index tail-elem)]
          (recur new-result (dec cnt)))))))

(defn combination
  "P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
  In how many ways can a committee of 3 be chosen from a group of 12 people? We
  all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial
  coefficients). For pure mathematicians, this result may be great. But we want to really
  generate all the possibilities in a list."

  [n lst]
; 1)
;  (let [sieves (gen-sieves (count lst) n)]
;    (for [s sieves] (sieve lst s))))

; 2)
  (let [sieves (gen-sieves (count lst) n)]
    (map sieve (repeat (count sieves) lst) sieves)))

