(ns advent-of-code.2015.day3
  (:require [advent-of-code.utils :as utils]
            [clojure.set :as set]))

(def instruction->direction
  {\^ [0 1]
   \> [1 0]
   \v [0 -1]
   \< [-1 0]})

(defn follow-direction
  [position direction]
  (map + position direction))

(defn houses-delivered-to
  [directions]
  (set (reductions follow-direction [0 0] directions)))

(defn part1-solution
  [instructions]
  (->> instructions
       (map instruction->direction)
       houses-delivered-to
       count))

(defn split-directions
  [directions]
  (reduce (fn [[a b] direction]
            [b (conj a direction)])
          [[] []]
          directions))

(defn part2-solution
  [instructions]
  (->> instructions
       (map instruction->direction)
       split-directions
       (map houses-delivered-to)
       (apply set/union)
       count))

(defn day-solution
  []
  (utils/with-lines "2015/day3.txt"
    (fn [[instructions]]
      (utils/tap (part1-solution instructions))
      (utils/tap (part2-solution instructions))))
  nil)

(comment
  (split-directions [1 2 3 4 5 6])
  (day-solution))
