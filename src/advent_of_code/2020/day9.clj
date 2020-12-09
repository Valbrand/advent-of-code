(ns advent-of-code.2020.day9
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]))

(defn part1-solution
  [lines]
  (let [window-size 25
        numbers (map utils/parse-int lines)]
    (->> numbers
         (drop window-size)
         (map-indexed vector)
         (drop-while (fn [[to-drop n]]
                       (some? (numbers/pair-summing
                               n
                               (->> numbers (drop to-drop) (take window-size))))))
         first
         second)))

(defn day-solution
  []
  (utils/with-lines "2020/day9.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))))
  nil)

(comment
  (day-solution))