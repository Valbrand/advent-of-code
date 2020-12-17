(ns advent-of-code.2020.day15
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]))

(defn init-past-numbers
  [numbers]
  (->> numbers
       (map-indexed vector)
       (reduce (fn [result [idx n]]
                 (update result n (fnil conj []) idx))
               {})))

(defn difference-between-last-two-elements
  [v]
  (let [before-last-idx (- (count v) 2)]
    (- (peek v)
       (nth v before-last-idx))))

(defn last-number-after-round
  [round numbers]
  (->> (range round)
       (drop (count numbers))
       (reduce (fn [{:keys [past-numbers last-number]} idx]
                 (let [n-indices (get past-numbers last-number)
                       next-n (if (= 1 (count n-indices))
                                0
                                (difference-between-last-two-elements n-indices))]
                   {:past-numbers (update past-numbers next-n (fnil conj []) idx)
                    :last-number  next-n}))
               {:past-numbers (init-past-numbers numbers)
                :last-number  (peek numbers)})
       :last-number))

(defn part1-solution
  [numbers]
  (last-number-after-round 2020 numbers))

(defn part2-solution
  [numbers]
  (last-number-after-round 30000000 numbers))

(defn parse-input
  [line]
  (mapv utils/parse-int (str/split line #",")))

(defn day-solution
  []
  (utils/with-lines "2020/day15.txt"
    (fn [[line]]
      (let [numbers (parse-input line)]
        (utils/tap (part1-solution numbers))
        (utils/tap (part2-solution numbers)))))
  nil)

(comment
  1
  (day-solution))
