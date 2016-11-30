(ns magic-squares.core
  (:gen-class))

(defn lucas-candidates [n]
  (for [a (range 1 (+ n 1))
        b (range 1 (+ n 1))
        c (range 1 (+ n 1))
        :when (and (< a b (- c a)) (not= b (* 2 a)))]
    [a b c]))

(defn lucas-squares [n]
  (lazy-seq
    (for [[a b c] (lucas-candidates n) :let [d (- a b)]]
      [[(- c b) (+ a b c) (- c a)] [(- c d) c (+ c d)] [(+ a c) (- c (+ a b)) (+ b c)]])))

(defn pretty-print [square]
  (apply str (interpose \newline (map (fn [[a b c]] (format "%2d %2d %2d" a b c)) square))))

(defn check [[[a b c] [d e f] [g h i]]]
  (= (+ a b c) (+ d e f) (+ g h i) (+ a d g) (+ b e h) (+ c f i) (+ a e i) (+ c e g)))

(defn magic-number [[[a b c] & _]]
  (+ a b c))

;; trying Matt Parker's "extra magic" 2-digit 3x3 squares
;; counting letters in the numbers also produces a magic square
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

(defn- -count [n counts]
  (or (some (fn [[count- nums]] (when (nums n) count-)) counts) 0))
 
(defn count-letters [n] ;; assumption: 0 < n < 100
  (if (< n 20)
    (-count n letter-counts)
    (let [tens (quot n 10) ones (mod n 10)]
      (+ (-count tens tens-letter-counts) (-count ones letter-counts)))))

(defn count-square [square]
  (vec (map vec (for [row square] (map count-letters row)))))

(def parker-squares (filter (comp check count-square) (lucas-squares 99)))

(defn -main [& args]
  (println (pretty-print (first (lucas-squares 10)))))
