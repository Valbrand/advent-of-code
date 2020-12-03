(ns advent-of-code-2020.day3
  (:require [advent-of-code-2020.utils :as utils]))

(defn trees-found-with-slope
  [lines {:keys [right-slope down-slope]}]
  (let [positions-to-check (->> (range 0 (count lines) down-slope)
                                (map-indexed (fn [n row]
                                               [row (* n right-slope)])))
        position-contents (->> positions-to-check
                               (map (fn [[row column]]
                                      (let [row (nth lines row)
                                            row-length (count row)]
                                        (nth row (mod column row-length))))))]
    (->> position-contents
         (filter #{\#})
         count)))

(defn slope
  [right down]
  {:right-slope right
   :down-slope  down})

(defn part1-solution
  [lines]
  (trees-found-with-slope
   lines
   (slope 3 1)))

(defn part2-solution
  [lines]
  (let [slopes (map (partial apply slope)
                    [[1 1]
                     [3 1]
                     [5 1]
                     [7 1]
                     [1 2]])]
    (->> slopes
         (map (partial trees-found-with-slope lines))
         (apply *))))

(defn day-solution
  []
  (let [path "resources/day3.txt"]
    (utils/with-lines path
      (part1-solution lines)
      (part2-solution lines))))

(comment
  (range 0 10 2)
  (day-solution))