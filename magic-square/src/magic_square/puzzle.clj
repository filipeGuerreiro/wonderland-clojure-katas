(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn sum-rows [m]
  (map #(reduce + %) m))

(defn sum-cols [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])

(defn sum-diagonals [m]
  [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
   (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))])

(defn all-equal? [els] (apply = els))

(defn magic-square? [m]
  (let [cols (sum-cols m)
        rows (sum-rows m)
        diagonals (sum-diagonals m)]
    (and (all-equal? cols) (all-equal? rows) (all-equal? diagonals)
         (= (first cols) (first rows) (first diagonals)))))

(defn permutate [m value-bag]
  (if (empty? value-bag)
    m
    (for [v value-bag]
      (permutate (conj m v) (disj value-bag v)))))

(defn almost-flatten
  [x]
  (filter #(and (sequential? %) (not-any? sequential? %))
    (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x))))

(defn magic-square [values]
  (let [value-bag (into #{} values)
        res (permutate [] value-bag)]
    (->> (almost-flatten res)
         (map #(partition 3 %))
         (map #(vec (apply map vector %)))
         (filter magic-square?))))
