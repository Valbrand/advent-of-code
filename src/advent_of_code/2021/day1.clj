(ns advent-of-code.2021.day1
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]))

(defn count-increases
  [input]
  (first
   (reduce (fn [[increases previous] current]
             (if (> current previous)
               [(inc increases) current]
               [increases current]))
           [0 (first input)]
           (rest input))))

(defn part1-solution
  []
  (let [path "2021/day1.txt"]
    (utils/with-lines path
      (fn [lines]
        (let [input (->> lines
                         (map numbers/parse-int))]
          (count-increases input))))))

(defn window-at-index
  [coll index]
  (+ (nth coll index)
     (nth coll (+ 1 index))
     (nth coll (+ 2 index))))

(defn part2-solution
  []
  (let [path "2021/day1.txt"]
    (utils/with-lines path
      (fn [lines]
        (let [input (->> lines
                         (mapv numbers/parse-int))]
          (->> (range 0 (- (count input) 2))
               (map #(window-at-index input %))
               (count-increases)))))))

(comment
  (part1-solution)
  (part2-solution))