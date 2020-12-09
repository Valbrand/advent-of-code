(ns advent-of-code.2015.day1
  (:require [advent-of-code.utils :as utils]))

(def instruction->value
  {\( 1
   \) -1})

(defn part1-solution
  [instructions]
  (->> instructions
       (map instruction->value)
       (reduce + 0)))

(defn part2-solution
  [instructions]
  (->> instructions
       (map instruction->value)
       (reduce (fn [result value]
                 (conj result (+ value (peek result)))) 
               [0])
       (take-while #(>= % 0))
       count))

(defn day-solution
  []
  (utils/with-lines "2015/day1.txt"
    (fn [[instructions]]
      (utils/tap (part1-solution instructions))
      (utils/tap (part2-solution instructions))))
  nil)

(comment
  (day-solution))