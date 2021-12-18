(ns advent-of-code.2021.day11
  "Dumbo octopuses
   
   This is a version of Conway's Game of life.
   
   For part 1, maybe maintaining a queue of flashes
   and iterating over it processing the cascading flashes will do."
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(defn parse-input
  [lines]
  {:energy-map (mapv #(mapv numbers/char->int %) lines)})

(defn state-after-energy-increment
  [{:keys [energy-map] :as input}]
  (let [energy-map-after #break (mapv #(mapv inc %) energy-map)
        flash-positions #break (->> (utils/cartesian-product (range 10) (range 10))
                                    (filter (fn [position]
                                              (> (get-in energy-map-after position)
                                                 9)))
                                    set)]
    {:energy-map      energy-map-after
     :flash-positions flash-positions
     :flashes         (count flash-positions)}))

(defn state-after-step
  [state]
  (let [final-state (->> (iterate (fn state-after-step-iteration
                                    [{:keys [energy-map flash-positions performed-flash-positions flashes]}]
                                    (let [flash-position (first flash-positions)
                                          positions-to-increment (utils/surrounding-matrix-indices flash-position energy-map)
                                          updated-energy-map (reduce #(update-in %1 %2 inc) energy-map positions-to-increment)
                                          new-flash-positions (->> positions-to-increment
                                                                   (filter #(= 10 (get-in updated-energy-map %))))]
                                      {:energy-map      updated-energy-map
                                       :flash-positions (-> (into flash-positions new-flash-positions)
                                                            (disj flash-position))
                                       :performed-flash-positions (conj performed-flash-positions flash-position)
                                       :flashes         (+ flashes (count new-flash-positions))}))
                                  (assoc (state-after-energy-increment state)
                                         :performed-flash-positions
                                         #{}))
                         (drop-while (comp seq :flash-positions))
                         first)]
    (reduce (fn [state position]
              (update state :energy-map assoc-in position 0))
            final-state
            (:performed-flash-positions final-state))))

(defn part1-solution
  []
  (let [path "2021/day11.txt"]
    (utils/with-lines path
      (fn [lines]
        (transduce (comp (drop 1)
                         (take 100)
                         (map :flashes))
                   +
                   0
                   (->> lines
                        parse-input
                        (iterate state-after-step)))))))

(defn synchronized?
  [{:keys [energy-map]}]
  (every? #(every? zero? %) energy-map))

(defn part2-solution
  []
  (let [path "2021/day11.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             parse-input
             (iterate state-after-step)
             (drop 1)
             (take-while (complement synchronized?))
             count
             inc)))))

(comment
  (time (part1-solution))
  (time (part2-solution))

  *e)
