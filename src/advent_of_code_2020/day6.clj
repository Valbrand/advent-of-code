(ns advent-of-code-2020.day6
  (:require [advent-of-code-2020.utils :as utils]))

(defn group-yes-answers
  [group-answers]
  (->> group-answers
       (reduce into #{})
       count))

(defn part1-solution
  [lines]
  (->> lines
       utils/split-by-empty-lines
       (map group-yes-answers)
       (reduce + 0)))

(defn day-solution
  []
  (utils/with-lines "resources/day6.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))))
  nil)

(comment
  (->> '("a" "" "" "b") (map identity) (partition-by empty?))
  (day-solution))
