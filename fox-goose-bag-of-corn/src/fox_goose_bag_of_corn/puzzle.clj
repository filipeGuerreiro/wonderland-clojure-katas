(ns fox-goose-bag-of-corn.puzzle
  (:refer-clojure :exclude [==])
  　 (:require [clojure.core.logic :as l]))

(def start-pos [[#{:fox :goose :corn :you} #{:boat} #{}]])
(def end-pos [[#{} #{:boat} #{:you :fox :goose :corn}]])

(defn pick-one [el rst col]
  (l/conde
    [(l/conso el rst col)]
    [(l/fresh [not-taken maybe-taken rst-not-taken]
              (l/conso not-taken maybe-taken col)
              (l/conso not-taken rst-not-taken rst)
              (pick-one el rst-not-taken maybe-taken))]))

(defn lasto [steps step]
  (l/conde
    [(l/== steps [step])]

    [(l/fresh [el rst]
              (l/conso el rst steps)
              (lasto rst step))]))

; Assumes farmer is not there
(defn nothing-eaten [side]
  (l/conde
    [(l/== side [])]

    [(l/fresh [a]
              (l/== side [a]))]

    [(l/== side [:fox :beans])]

    [(l/== side [:beans :fox])]))

(defn all-safe [step]
  (l/fresh [farmer left right]
           (l/== step [farmer left right])
           (l/conde
             [(l/== farmer :left)
              (nothing-eaten right)]
             [(l/== farmer :right)
              (nothing-eaten left)])))

(defn transport [prev-step next-step]
  (l/fresh [item farmer1 farmer2 left1 left2 right1 right2]
           (l/== prev-step [farmer1 left1 right1])
           (l/== next-step [farmer2 left2 right2])
           (all-safe prev-step)
           (all-safe next-step)

           (l/conde
             ; Move item from left to right
             [(l/== farmer1 :left)
              (l/== farmer2 :right)
              (l/conso item right1 right2)
              (pick-one item left2 left1)]

             ; Move item from right to left
             [(l/== farmer1 :right)
              (l/== farmer2 :left)
              (l/conso item left1 left2)
              (pick-one item right2 right1)]

             ; Farmer returns without an item (forgot this case originally!)
             [(l/== farmer1 :right)
              (l/== farmer2 :left)
              (l/== left1 left2)
              (l/== right1 right2)]

             [(l/== farmer1 :left)
              (l/== farmer2 :right)
              (l/== left1 left2)
              (l/== right1 right2)])))

(defn transportations [steps]
  (l/fresh [a b _rst rst]
           (l/conde
             [(l/== steps [a])]

             [(transport a b)
              (l/conso a rst steps)
              (l/conso b _rst rst)
              (transportations rst)])))

(comment

  (first
    (l/run 1 [steps]
           (l/fresh [a b c d e f g h]
                    (l/== steps [a b c d e f g h])
                    (l/== a [:left [:fox :goose :beans] []])

                    (l/fresh [ff gg bb]
                             (l/== h [:right [] [ff gg bb]])) ; Can't hard code order of stuff at end

                    (transport a b)
                    (transport b c)
                    (transport c d)
                    (transport d e)
                    (transport e f)
                    (transport f g)
                    (transport g h))))

  (first
    (l/run 1 [steps]
           (l/fresh [ff gg bb]
                    (l/firsto steps [:left [:fox :goose :beans] []])
                    (lasto steps [:right [] [ff gg bb]])
                    (transportations steps)))))
