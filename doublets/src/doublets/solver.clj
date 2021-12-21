(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))
(def word-set (into #{} words))

(defn differ-by-one-ch [word1 word2]
  (and (= (count word1) (count word2))
       (= 1
          (apply +
                 (for [i (range (count word1))]
                   (if (not= (nth word1 i) (nth word2 i))
                     1
                     0))))))

(defn get-words-differ-by-one-ch [word word-set]
  (->> word-set (filter #(differ-by-one-ch word %))))

(defn almost-flatten
  [x]
  (filter #(and (sequential? %) (not-any? sequential? %))
    (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x))))

(defn- doubletsp [word1 word2 word-set res]
  (if (= word1 word2)
    res
    (for [next-word (get-words-differ-by-one-ch word1 word-set)]
        (doubletsp next-word word2 (disj word-set next-word) (conj res next-word)))))

(defn doublets [word1 word2]
  (let [res (doubletsp word1 word2 word-set (list word1))]
    (->> (almost-flatten res)
         (filter seq)
         (apply min-key count)
         (reverse))))
