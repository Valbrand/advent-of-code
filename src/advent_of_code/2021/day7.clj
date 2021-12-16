(ns advent-of-code.2021.day7
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(defn parse-input
  [[line]]
  (utils/parse-integers-line line #","))

(defn compute-required-fuel
  ([positions]
   (compute-required-fuel identity positions))
  ([fuel-fn positions]
   (let [position->crabs-count (frequencies positions)]
     (reduce (fn [distance-map position-a]
               (reduce
                (fn [distance-map position-b]
                  (let [fuel-for-distance (fuel-fn (numbers/abs (- position-a position-b)))
                        fuel-needed-a->b (* (get position->crabs-count position-a)
                                            fuel-for-distance)
                        fuel-needed-b->a (* (get position->crabs-count position-b)
                                            fuel-for-distance)]
                    (-> distance-map
                        (update position-a + fuel-needed-b->a)
                        (update position-b + fuel-needed-a->b))))
                (assoc distance-map position-a 0)
                (keys distance-map)))
             {}
             (keys position->crabs-count)))))

(defn part1-solution
  []
  (let [path "2021/day7.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             parse-input
             compute-required-fuel
             vals
             (apply min))))))

(defn fuel-needed-for-crab-sub-movement
  [distance]
  (/ (* distance (inc distance))
     2))

(defn part2-solution
  []
  (let [path "2021/day7.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             parse-input
             (compute-required-fuel fuel-needed-for-crab-sub-movement)
             vals
             (apply min))))))



(comment
  (part1-solution)
  (part2-solution)

  *e)
