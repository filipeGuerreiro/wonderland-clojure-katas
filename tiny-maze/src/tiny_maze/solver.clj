(ns tiny-maze.solver)

(defn end? [move] (= :E move))
(defn passable? [pos maze]
  (let [res (get-in maze pos)]
    (or (= 0 res)
        (end? res))))

(defn available-moves [pos maze]
  (let [up [(dec (first pos)) (second pos)]
        down [(inc (first pos)) (second pos)]
        right [(first pos) (inc (second pos))]
        left [(first pos) (dec (second pos))]]
    (filter #(passable? % maze) [down right left up])))

(defn almost-flatten
  [x]
  (filter #(and (sequential? %) (not-any? sequential? %))
    (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x))))

(defn solve-maze
  ([maze]
   (->> (solve-maze (assoc-in maze [0 0] :x) [0 0])
        (almost-flatten)
        (filter seq)))
  ([maze pos]
   (for [next-pos (available-moves pos maze)
         :let [move (get-in maze next-pos)
               next-maze (assoc-in maze next-pos :x)]]
      (if (end? move)
        next-maze
        (solve-maze next-maze next-pos)))))
