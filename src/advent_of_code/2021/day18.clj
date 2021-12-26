(ns advent-of-code.2021.day18
  "Snailfish"
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.zip :as zip]))

(defn parse-snailfish-number
  [line]
  (edn/read-string line))

(defn parse-input
  [lines]
  (map parse-snailfish-number lines))

(defn snailfish-number-zipper
  [snailfish-number]
  (zip/vector-zip snailfish-number))

(defn safe-branch? [loc]
  (if (or (nil? loc)
          (zip/end? loc))
    false
    (zip/branch? loc)))

(defn root-loc [loc]
  (->> (iterate zip/up loc)
       (take-while some?)
       last))

(defn number-to-the-left
  [loc]
  (->> (iterate zip/prev loc)
       (drop 1)
       (drop-while safe-branch?)
       first))

(defn number-to-the-right
  [loc]
  (if (zip/branch? loc)
    (recur (-> loc zip/down zip/rightmost))
    (let [right-loc (->> (iterate zip/next loc)
                         (drop 1)
                         (drop-while safe-branch?)
                         first)]
      (if (zip/end? right-loc)
        nil
        right-loc))))

(defn loc-seq
  [loc]
  (->> (iterate zip/next loc)
       (take-while (complement zip/end?))))

(defn depth
  [loc]
  (->> (iterate zip/up loc)
       (drop 1)
       (take-while some?)
       count))

(defn simple-pair?
  [n]
  (and (vector? n)
       (every? number? n)))

(defn loc-to-explode
  [n-loc]
  (->> n-loc
       loc-seq
       (filter (comp simple-pair? zip/node))
       (utils/find-first #(>= (depth %) 4))))

(defn explode-node
  [loc]
  (let [[l r] (zip/node loc)]
    (root-loc
     (cond-> loc
       :always
       (zip/edit (constantly 0))

       (some-> loc
               number-to-the-left)
       (-> number-to-the-left
           (zip/edit + l)
           number-to-the-right)

       (some-> loc
               number-to-the-right)
       (-> number-to-the-right
           (zip/edit + r))))))

(defn loc-to-split
  [n-loc]
  (->> n-loc
       loc-seq
       (filter (comp number? zip/node))
       (utils/find-first #(>= (zip/node %) 10))))

(defn split-node
  [loc]
  (let [n (zip/node loc)
        left (quot n 2)
        right (+ (quot n 2)
                 (mod n 2))]
    (root-loc
     (zip/edit loc (constantly [left right])))))

(defn reduce-snailfish-number
  [n-loc]
  (if-let [exploding-loc (loc-to-explode n-loc)]
    (recur (explode-node exploding-loc))
    (if-let [splitting-loc (loc-to-split n-loc)]
      (recur (split-node splitting-loc))
      n-loc)))

(defn add
  [n1 & numbers]
  (reduce (fn [result n]
            (let [sum-before-reduction [result n]]
              (-> sum-before-reduction
                  snailfish-number-zipper
                  reduce-snailfish-number
                  zip/root)))
          n1
          numbers))

(defn magnitude
  [n]
  (walk/postwalk (fn [number-or-pair]
                   (if (number? number-or-pair)
                     number-or-pair
                     (+ (* 3 (first number-or-pair))
                        (* 2 (second number-or-pair)))))
                 n))

(defn part1-solution
  []
  (utils/with-input
    (fn [lines]
      (->> lines
           parse-input
           (apply add)
           magnitude))))

(defn largest-possible-sum-of-two-numbers
  [numbers]
  (let [numbers (set numbers)]
    (apply max
           (for [n1 numbers
                 n2 (disj numbers n1)]
             (magnitude (add n1 n2))))))

(defn part2-solution
  []
  (utils/with-input
    (fn [lines]
      (->> lines
           parse-input
           largest-possible-sum-of-two-numbers))))

(comment
  (time (part1-solution))
  (time (part2-solution))

  *e)
