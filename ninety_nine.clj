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

(defn encode
  "P10 (*) Run-length encoding of a list.
  Use the result of problem P09 to implement the so-called run-length encoding
  data compression method. Consecutive duplicates of elements are encoded as
  lists (N E) where N is the number of duplicates of the element E."
  { :_test '(= (encode '(a a a a b c c a a d e e e e)) '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))) }

  [lst]
  (for [sublist (pack lst)] (list (my-count sublist) (first sublist))))


(defn my-repeat
  "A helper function similar to the built-in repeat.
  Note that the built-in repeat is lazy and much more elegant."
  { :_test '[(= (my-repeat 'a) '(a a))
             (= (my-repeat 10 1) '(1 1 1 1 1 1 1 1 1 1))] }

  ([a] (my-repeat 2 a))
  ([n a] (for [_ (range n)] a)))

(defn encode-modified
  "P11 (*) Modified run-length encoding.
  Modify the result of problem P10 in such a way that if an element has no
  duplicates it is simply copied into the result list. Only elements with
  duplicates are transferred as (N E) lists."
  { :_test '(= (encode-modified '(a a a a b c c a a d e e e e)) '((4 a) b (2 c) (2 a) d (4 e))) }

  [lst]
  (for [sublist (encode lst)] (let [[cnt elem] sublist] (if (= cnt 1) elem sublist))))

(defn decode
  "P12 (**) Decode a run-length encoded list.
  Given a run-length code list generated as specified in problem P11. Construct
  its uncompressed version."
  { :_test '(= (decode '((4 a) b (2 c) (2 a) d (4 e))) '(a a a a b c c a a d e e e e)) }

  [lst]
  (loop [ls lst, result ()]
    (if (empty? ls)
      result
      (recur (rest ls)
             (if (list? (first ls))
               (let [[cnt elem] (first ls)]
                 (concat result (my-repeat cnt elem)))
               (concat result (list (first ls))))))))

; TODO: P13

(defn dupli
  "P14 (*) Duplicate the elements of a list."
  { :_test '(= (dupli '(a b c c d)) '(a a b b c c c c d d)) }

  [lst]
  (loop [ls lst, result ()]
    (if (empty? ls)
      result
      (recur (rest ls) (concat result (my-repeat 2 (first ls)))))))

(defn repli
  "P15 (**) Replicate the elements of a list a given number of times."
  { :_test '(= (repli '(a b c) 3) '(a a a b b b c c c)) }

  [lst n]
  (apply concat (for [item lst] (my-repeat n item))))

(defn dropn
  "P16 (**) Drop every N'th element from a list."
  { :_test '(= (dropn '(a b c d e f g h i k) 3) '(a b d e g h k)) }

  [lst n]
  (loop [ls lst, cnt 0, result ()]
    (if (empty? ls)
      (my-reverse result)
      (recur (rest ls)
             (mod (inc cnt) n)
             (if (= cnt (dec n))
               result
               (cons (first ls) result))))))

(defn my-split-at
  "P17 (*) Split a list into two parts; the length of the first part is given.
  Do not use any predefined predicates."
  { :_test '(= (my-split-at '(a b c d e f g h i k) 3)
               '((a b c) (d e f g h i k))) }

  [lst k]
  (list (take k lst) (drop k lst)))

(defn slice
  "P18 (**) Extract a slice from a list.
  Given two indices, I and K, the slice is the list containing the elements
  between the I'th and K'th element of the original list (both limits
  included). Start counting the elements with 1."
  { :_test '(= (slice '(a b c d e f g h i k) 3 7)
               '(c d e f g)) }

  [lst i k]
  (take (inc (- k i)) (drop (dec i) lst)))

(defn rotate
  "P19 (**) Rotate a list N places to the left."
  { :_test '[(= (rotate '(a b c d e f g h) 3)
                '(d e f g h a b c))
             (= (rotate '(a b c d e f g h) -2)
                '(g h a b c d e f))] }


  [lst n]
  (let [offset (if (pos? n) n (+ (count lst) n)),
        [left right] (my-split-at lst offset)]
    (concat right left)))

(defn remove-at
  "P20 (*) Remove the K'th element from a list."
  { :_test '(= (remove-at '(a b c d) 2)
               '(a c d)) }

  [lst k]
  (concat (take (dec k) lst) (drop k lst)))


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
