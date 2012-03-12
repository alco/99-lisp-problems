(ns ninety-nine
  (:require clojure.test))

;;
;; Working with lists
;;

(defn my-last
  "P01 (*) Find the last box of a list."
  { :_test '(= (my-last '(a b c d)) 'd) }

  [lst]
  (if (<= (count lst) 1)
    (first lst)
    (recur (rest lst))))

(defn my-but-last
  "P02 (*) Find the last but one box of a list."
  { :_test '(= (my-but-last '(a b c d)) '(c d)) }

  [lst]
  (if (<= (count lst) 2)
    lst
    (recur (rest lst))))

(defn element-at
  "P03 (*) Find the K'th element of a list.
  The first element in the list is number 1."
  { :_test '(= (element-at '(a b c d e) 3) 'c) }

  [lst index]
  (if (= index 1)
    (first lst)
    (recur (rest lst) (dec index))))

(defn my-count
  "P04 (*) Find the number of elements of a list."
  { :_test '(= (my-count '(a b c d e)) 5) }

  [lst]
  (loop [ls lst, cnt 0]
    (if (empty? ls)
      cnt
      (recur (rest ls) (inc cnt)))))

(defn my-reverse
  "P05 (*) Reverse a list."
  { :_test '(= (my-reverse '(a b c d)) '(d c b a)) }

  [lst]
  (loop [ls lst, result ()]
    (if (empty? ls)
      result
      (recur (rest ls) (cons (first ls) result)))))

(defn palindrome?
  "P06 (*) Find out whether a list is a palindrome.
  A palindrome can be read forward or backward; e.g. (x a m a x)."
  { :_test '(true? (palindrome? '(a b b c d e d c b b a))) }

  [lst]
  (loop [norm lst, rev (my-reverse lst)]
    (if (empty? norm)
      true
      (if (not= (first norm) (first rev))
        false
        (recur (rest norm) (rest rev))))))

(defn my-flatten
  "P07 (**) Flatten a nested list structure.
  Transform a list, possibly holding lists as elements into a `flat' list
  by replacing each list with its elements (recursively)."
  { :_test '(= (my-flatten '(a (b (c d) e))) '(a b c d e)) }

  [lst]
  (loop [ls lst, result ()]
    (if (empty? ls)
      result
      (let [head (first ls), tail (rest ls)]
        (recur tail (concat result (if (coll? head) (my-flatten head) (list head))))))))


;;
;; Generate tests from the functions' metadata
;;
(defmacro gen-tests []
  `(do ~@(for [f (map first (ns-publics *ns*)) :when (:_test (meta (ns-resolve *ns* (symbol f))))]
           (let [tests# (:_test (meta (ns-resolve *ns* (symbol f))))]
             (if (vector? tests#)
               `(do ~@(for [tst# (map-indexed (fn [i x] [i x]) tests#)]
                        `(clojure.test/deftest ~(symbol (str f '-test- (first tst#))) (clojure.test/is ~(second tst#)))))
               `(clojure.test/deftest ~(symbol (str f '-test)) (clojure.test/is ~tests#)))))))

(gen-tests)


;; Expose the run-tests function to the host namespace
(let [cur-ns *ns*]
  (defn run-tests []
    (clojure.test/run-tests cur-ns)))
