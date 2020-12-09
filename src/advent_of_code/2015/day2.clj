(ns advent-of-code.2015.day2
  (:require [advent-of-code.utils :as utils]))

(def dimensions-pattern #"(\d+)x(\d+)x(\d+)")

(defn parse-line
  [line]
  (let [[_ l w h] (re-find dimensions-pattern line)]
    (map utils/parse-int [l w h])))

(defn paper-required
  [dimensions]
  (let [[smallest-side :as all-sides] (sort
                                       (for [a (range 3)
                                             b (range (inc a) 3)]
                                         (* (nth dimensions a)
                                            (nth dimensions b))))]
    (->> all-sides
         (map #(* 2 %))
         (reduce + smallest-side))))

(defn part1-solution
  [lines]
  (->> lines
       (map parse-line)
       (map paper-required)
       (reduce + 0)))

(defn ribbon-required
  [dimensions]
  (let [[smallest-dim second-smallest-dim] (sort dimensions)
        wrapping-requirement (->> [smallest-dim second-smallest-dim]
                                  (map #(* 2 %))
                                  (reduce + 0))
        bow-requirement (reduce * 1 dimensions)]
    (+ wrapping-requirement bow-requirement)))

(defn part2-solution
  [lines]
  (->> lines
       (map parse-line)
       (map ribbon-required)
       (reduce + 0)))

(defn day-solution
  []
  (utils/with-lines "2015/day2.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))
      (utils/tap (part2-solution lines))))
  nil)

(comment
  (day-solution))
