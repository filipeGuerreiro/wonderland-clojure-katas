(ns 4clojure-exercises.core)

(defn group-anagrams [ss]
  (->> ss
       (group-by #(apply str (sort %)))
       vals
       (map set)
       set))
  (= (group-anagrams ["meat" "mat" "team" "mate" "eat"]) #{#{"meat" "team" "mate"}})
  (= (group-anagrams ["veer" "lake" "item" "kale" "mite" "ever"])
     #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

  (defn divisible? [a b]
    (zero? (mod a b)))
  (defn prime? [n]
    (and (> n 1) (not-any? (partial divisible? n) (range 2 n))))
  (defn generate-primes [n]
    (take n ((fn generate [a] (lazy-seq (if (prime? a) (cons a (generate (inc a))) (generate (inc a))))) 2)))
  (= (generate-primes 5) [2 3 5 7 11])

  (defn split-by-type [ss]
    (->> ss
         (group-by class)
         vals))
  (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})

  (defn split-seq [s n]
    (let [ps (partition n s)
          psize (/ (count s) (count ps))]
      (for [k (range psize)]
        (map #(nth % k) ps))))
  (= (split-seq [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
  (= (split-seq (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

  (defn rotate-seq [n ss]
    (let [cut-point (mod n (count ss))]
      (concat (subvec ss cut-point)
              (subvec ss 0 cut-point))))
  (= (rotate-seq 6 [1 2 3 4 5]) '(2 3 4 5 1))

  (defn flip-args-fn [fun]
    (fn [& args]
      (apply fun (reverse args))))
  (= true ((flip-args-fn >) 7 8))
  (= 4 ((flip-args-fn quot) 2 8))


  (defn sym? [[_ l r]]
    (letfn [(mirror [[n l r]]
              [n
               (if (sequential? r) (mirror r) r)
               (if (sequential? l) (mirror l) l)])]
      (= (mirror r) l)))
  (= (sym? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]] [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]]) true)
  (= (sym? '(:a (:b nil nil) (:b nil nil))) true)
  (= (sym? '(:a (:b nil nil) nil)) false)

  (defn bin-tree? [ss]
    (if (sequential? ss)
      (if (= 3 (count ss))
        (and (bin-tree? (second ss))
             (bin-tree? (last ss)))
        false)
      (nil? ss)))
  (= (bin-tree? '(:a (:b nil nil))) false)
  (= (bin-tree? '(:a (:b nil nil) nil)) true)

  (defn pascal-row [n]
    (fn calc-row [prev-row]
      (as-> prev-row $
        (partition 2 1 $)
        (map #(apply + %) $)
        (concat '(1) $ '(1))
        (into [] $)))
    (loop [row [1]
           i 1]
      (if (= n i)
        row
        (recur (calc-row row) (inc i)))))
  (pascal-row 4)
  (= (map pascal-row (range 1 6))
     [[1]
      [1 1]
      [1 2 1]
      [1 3 3 1]
      [1 4 6 4 1]])

  (defn lcm
    ([a b & c] (reduce lcm (lcm a b) c))
    ([a b]
     (->> (iterate #(+ % a) a)
          (filter #(zero? (rem % b)))
          first)))
  (== (lcm 7 5/7 2 3/5) 210)

  (def a #{1 2 3 4 5 6})
  (def b #{1 3 5 7})
  (defn sym-dif [a b]
    (fn uniques [c d] (into #{} (filter #(not-any? #{%} d) c)))
    (let [a-uniques (uniques a b)
          b-uniques (uniques b a)]
      (into #{} (concat a-uniques b-uniques))))
  (= (sym-dif #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})

  (def a #{"ace" "king" "queen"})
  (def b #{"♠" "♥" "♦" "♣"})
  (defn cartesian [a b]
    (into #{} (mapcat (fn [ai] (map (fn [bj] [ai bj]) b)) a)))
  (= (cartesian #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
     #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
       ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
       ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})

  (defn exp [x] (fn [n] (int (java.lang.Math/pow n x))))
  (= [1 8 27 64] (map (exp 3) [1 2 3 4]))

  (def a 1023)
  (def b 858)
  (defn gcd [a b]
    (if (= b 0)
      a
      (gcd b (mod a b))))
  (= (gcd 1023 858) 33)

  (def preds '(false false))
  (defn x-some? [& preds]
    (if (seq? preds)
      (some? (and (some true? preds) (some false? preds)))
      false))
  (= false (x-some? false false))

  (= [1 2 [3 4 5] [1 2 3 4 5]]
     (let [[a b & c :as d] [1 2 3 4 5]]
       [a b c d]))

  (= [2 4] (let [[a b c d e f g] (range)] [c e]))

  (defn factorial [n]
    (if (< n 2)
      1
      (* n (factorial (dec n)))))
  (= (factorial 5) 120)

  (def s "HeLlO, WoRlD!")
  (defn grab-capitals [s] (reduce #(str %1 %2) (re-seq #"[A-Z]+" s)))
  (= (grab-capitals "HeLlO, WoRlD!") "HLOWRD")

  (defn fib [x]
    (take x ((fn fib-in [a b] (lazy-seq (cons a (fib-in b (+ a b))))) 1 1)))
  (def fib-seq-seq
    ((fn fib [a b]
       (lazy-seq (cons a (fib b (+ a b)))))
     0 1))
  (take 30 fib-seq-seq)
  (= (__ 8) '(1 1 2 3 5 8 13 21))
  (= ((fn [ss times] (apply concat (map #(repeat times %) ss))) [1 2 3] 2) '(1 1 2 2 3 3))
  (repeat 2 [1])
  (= '(1 4 7 10 13) (take 5 (iterate #(+ 3 %) 1)))
  (defn drop-every [n xs]
    (lazy-seq
     (when (seq xs)
       (concat (take (dec n) xs)
               (drop-every n (drop n xs))))))
  (drop-every 3 [1 2 3 4 5 6 7 8])
  (= (drop-every 3 [1 2 3 4 5 6 7 8]) [1 2 4 5 7 8])
  ((fn [ss nth] (partition nth ss)) [1 2 3 4 5 6 7 8] 3)
  (map #(first %) (partition-by (fn [el] el) [1 1 2 3 3 2 2 3]))
  (partition-by (fn [el] el) [1 1 2 1 1 1 3 3])
  (= ((fn [ss] (map #(first %) (partition-by (fn [el] el) ss))) [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
  (apply concat (map #(conj [%] %) [[1 2] [3 4]]))
  (= ((fn [ss] (->> ss (map #(conj [%] %)) (apply concat) )) [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
  (true? ((fn [ss] (= (count ss) (count (for [a (seq ss)
                                              b (reverse a)
                                              :while (= a b)] a)))) "racecar"))
  (= ((fn [ls] (filter #(not= 0 (mod % 2)) ls)) #{1 2 3 4 5}) '(1 3 5))
  (= ((fn [ls] (reduce #(+ %1 %2) ls)) [1 2 3]) 6)
  (= ((fn [ls] (first (take-last 2 ls))) (list 1 2 3 4 5)) 4)

  (= '(1 5 9 13 17 21 25 29 33 37) (for [x (iterate #(+ 4 %) 0)
                                         :let [z (inc x)]
                                         :while (< z 40)]
                                     z))
  (= (last (sort (rest (reverse [2 5 4 1 3 6]))))
     (-> [2 5 4 1 3 6] reverse rest sort last)
     5)
  (= '(5 4 3 2 1) ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))
  (= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))
  (= 7 (let [x 5] (+ 2 x)))
  (= ((fn [who] (str "Hello, " who "!")) "Dave") "Hello, Dave!")
  (= ((fn [n] (* 2 n)) 2) 4)
  (= 8 (#(+ % 5) 3))
  (= {:a 1, :b 2, :c 3} (conj {:a 1} {:b 2} [:c 3]))
  (= 20 ((hash-map :a 10, :b 20, :c 30) :b))
  (= #{1 2 3 4} (conj #{1 4 3} 2))
  (= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))
  (= '(1 2 3 4) (conj '(2 3 4) 1))
  (= (list :a :b :c) '(:a :b :c))
  (= "HELLO WORLD" (.toUpperCase "hello world"))
