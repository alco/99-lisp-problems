(ns ninety-nine
  (:require clojure.test))

;;
;; Working with lists
;;

; Clojure has built-ins for some of the functions defined below. They are
; mentioned in the docstrings where appropriate.

(defn my-last
  "P01 (*) Find the last box of a list.

  *Built-in: last"
  { :_test '[(= (my-last '(a b c d)) 'd)
             (= (my-last '(a b c)) 'c)
             (= (my-last ()) nil)
             (= (my-last '(w h a t e v e r)) (last '(w h a t e v e r)))] }

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
  The first element in the list is number 1.

  *Built-in: nth"
  { :_test '[(= (element-at '(a b c d e) 3) 'c)
             (= (element-at '(a b c) 1) 'a)
             (= (element-at '(a b c) 2) (nth '(a b c) 1))] }

  [lst index]
  (if (= index 1)
    (first lst)
    (recur (rest lst) (dec index))))

(defn my-count
  "P04 (*) Find the number of elements of a list.

  *Built-in: count"
  { :_test '[(= (my-count '(a b c d e)) 5)
             (= (my-count ()) 0)
             (= (my-count '(w h a t e v e r)) (count '(w h a t e v e r)))] }

  [lst]
  (loop [ls lst, cnt 0]
    (if (empty? ls)
      cnt
      (recur (rest ls) (inc cnt)))))

(defn my-reverse
  "P05 (*) Reverse a list.

  *Built-in: reverse"
  { :_test '[(= (my-reverse '(a b c d)) '(d c b a))
             (= (my-reverse '(w h a t e v e r)) (reverse '(w h a t e v e r)))] }

  [lst]
  (loop [ls lst, result ()]
    (if (empty? ls)
      result
      (recur (rest ls) (cons (first ls) result)))))

(defn palindrome?
  "P06 (*) Find out whether a list is a palindrome.
  A palindrome can be read forward or backward; e.g. (x a m a x)."
  { :_test '[(true? (palindrome? '(a b b c d e d c b b a)))
             (true? (palindrome? ()))
             (true? (palindrome? '(a)))
             (false? (palindrome? '(a b)))] }

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
  { :_test '[(= (my-flatten '(a (b (c d) e))) '(a b c d e))
             (= (my-flatten '(a b (c ()))) '(a b c))
             (= (my-flatten '(((())))) ())] }

  [lst]
  (loop [ls lst, result ()]
    (if (empty? ls)
      result
      (let [head (first ls), tail (rest ls)]
        (recur tail (concat result (if (coll? head) (my-flatten head) (list head))))))))

(defn compress
  "P08 (**) Eliminate consecutive duplicates of list elements.
  If a list contains repeated elements they should be replaced with a single
  copy of the element. The order of the elements should not be changed."
  { :_test '(= (compress '(a a a a b c c a a d e e e e)) '(a b c a d e)) }

  [lst]
  (loop [ls lst, result []]
    (if (empty? ls)
      result
      (recur (rest ls)
             (if (= (first ls) (last result))
               result
               (conj result (first ls)))))))

(defn pack
  "P09 (**) Pack consecutive duplicates of list elements into sublists.
  If a list contains repeated elements they should be placed in separate sublists."
  { :_test '[(= (pack '(a a a a b c c a a d e e e e))
                '((a a a a) (b) (c c) (a a) (d) (e e e e)))
             (= (pack '(a b c d)) '((a) (b) (c) (d)))
             (= (pack ()) ())
             (= (pack '(a nil ())) '((a) (nil) (())))] }

  [lst]
  (loop [ls lst, result ()]
    (if (empty? ls)
      (my-reverse result)
      (let [head (first ls),
            result-head (first result)]
        (recur (rest ls)
               (if (= (first result-head) head)
                 (cons (cons head result-head) (rest result))
                 (cons (list head) result)))))))


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
