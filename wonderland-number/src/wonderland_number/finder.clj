(ns wonderland-number.finder)

(defn six-digits? [n]
  (= 6 (count (str n))))
(defn same-digits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))
(defn wondernum? [n]
  (and
   (six-digits? n)
   (same-digits? n (* n 2))
   (same-digits? n (* n 3))
   (same-digits? n (* n 4))
   (same-digits? n (* n 5))
   (same-digits? n (* n 6))))

(defn wonderland-number []
  (loop [n 100000]
    (if (not (wondernum? n))
      (recur (inc n))
      n)))
