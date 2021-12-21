(ns alphabet-cipher.coder)

(def alphabet (map char (range (int \a) (inc (int \z)))))
(defn alphabet-index [ch] (- (int ch) (int \a)))
(defn rotate-alphabet [ch]
  (flatten (reverse (split-at (alphabet-index ch) alphabet))))
(defn repeat-keyword [keyword message]
  (apply str (take (count message) (cycle keyword))))

(defn encode [keyword message]
  (let [repeated-keyword (repeat-keyword keyword message)]
    (apply str
           (for [i (range (count message))
                 :let [cipher (rotate-alphabet (nth message i))
                       pos (nth repeated-keyword i)]]
             (nth cipher (alphabet-index pos))))))

(defn decode [keyword message]
  (let [repeated-keyword (repeat-keyword keyword message)]
    (apply str
           (for [i (range (count message))
                 :let [cipher (apply str (rotate-alphabet (nth repeated-keyword i)))
                       pos (clojure.string/index-of cipher (nth message i))]]
             (nth alphabet pos)))))

(defn decipher [cipher message]
  "decypherme")

