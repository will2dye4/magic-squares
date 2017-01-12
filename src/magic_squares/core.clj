(ns magic-squares.core
  (:gen-class))

;; Functions for generating and testing magic squares
;; of order 3, due to the work of Edouard Lucas
;; See: https://en.wikipedia.org/wiki/Magic_square#Method_for_constructing_a_magic_square_of_order_3
(defn lucas-candidates 
  "Returns a seq of 3-item vectors [a b c] describing all candidates for
  magic squares where a + b + c <= n using Lucas' method for squares of order 3."
  [n]
    {:pre [(pos? n)]}
    (for [a (range 1 (+ n 1))
          b (range (+ a 1) (+ n 1))
          c (range (+ a b 1) (+ n 1))
          :when (and (< a b (- c a)) (not= b (* 2 a)) (<= (+ a b c) n))]
      [a b c]))

(defn lucas-squares
  "Returns a lazy seq of 2D 3x3 vectors describing all magic squares for
  integers up to n (inclusive) using Lucas' method for squares of order 3."
  [n]
    {:pre [(pos? n)]}
    (lazy-seq
      (for [[a b c] (lucas-candidates n) :let [d (- a b)]]
        [[(- c b) (+ a b c) (- c a)] [(- c d) c (+ c d)] [(+ a c) (- c (+ a b)) (+ b c)]])))

(defn pretty-print
  "Returns a formatted string representation of the given 3x3 square."
  [square]
    (apply str (interpose \newline (map (fn [[a b c]] (format "%2d %2d %2d" a b c)) square))))

(defn check
  "Checks the given 3x3 square to see if it is a magic square."
  [[[a b c] [d e f] [g h i]]]
    (= (+ a b c) (+ d e f) (+ g h i) (+ a d g) (+ b e h) (+ c f i) (+ a e i) (+ c e g)))

(defn magic-number
  "Returns the magic number associated with the given 3x3 magic square."
  [[[a b c] & _]]
    (+ a b c))

;; Trying Matt Parker's "extra magic" 2-digit 3x3 squares, where
;; counting the letters in the numbers produces another magic square
;; See: https://www.youtube.com/watch?v=cZ1W1vbuYuQ
(def letter-counts
  {3 #{1 2 6 10}
   4 #{4 5 9}
   5 #{3 7 8}
   6 #{11 12}
   7 #{15 16}
   8 #{13 14 18 19}
   9 #{17}})

(def tens-letter-counts
  {5 #{4 5 6}
   6 #{2 3 8 9}
   7 #{7}})

(defn- -count
  "Given a number n and one of the letter-count maps above,
  returns the letter count associated with n or zero if not found."
  [n counts] (or (some (fn [[count- nums]] (when (nums n) count-)) counts) 0))
 
(defn count-letters 
  "Returns the number of letters in the English spelling of the integer n."
  [n]
    {:pre [(< 0 n 100)]}
    (if (< n 20)
      (-count n letter-counts)
      (let [tens (quot n 10) ones (mod n 10)]
        (+ (-count tens tens-letter-counts) (-count ones letter-counts)))))

(defn count-square
  "Given a 3x3 magic square, returns another 3x3 square where
  each number in the input square is replaced by its letter count."
  [square] (vec (map vec (for [row square] (map count-letters row)))))

;; the 3x3 magic squares that also have the Matt Parker letter-count property
(def parker-squares (filter (comp check count-square) (lucas-squares 99)))

(defn -main
  "Prints all of the 3x3 'Parker squares' to stdout."
  [& args]
    (println "There are" (count parker-squares) "3x3 magic squares with magic square letter counts.")
    (doseq [s parker-squares] (println (pretty-print s) \newline)))

