(ns advent-of-code.2021.day5
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(defn parse-line
  [line]
  (->> (string/split line #"\s+->\s+")
       (map #(into [] (utils/parse-integers-line % #",")))))

(defn horizontal-segment?
  [[[_ y1] [_ y2]]]
  (= y1 y2))

(defn vertical-segment?
  [[[x1 _] [x2 _]]]
  (= x1 x2))

(defn coordinates-range
  [a b]
  (if (<= a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn points-in-segment
  [[[x1 y1] [x2 y2] :as segment]]
  (cond
    (vertical-segment? segment)
    (utils/zip (repeat x1) (coordinates-range y1 y2))

    (horizontal-segment? segment)
    (utils/zip (coordinates-range x1 x2) (repeat y1))
    
    :else
    (utils/zip (coordinates-range x1 x2)
               (coordinates-range y1 y2))))

(def empty-vents-map
  {:vents {}
   :dangerous-spots 0})

(defn track-segment
  [vents-map segment]
  (reduce (fn track-segment-inner [vents-map-acc point]
            (if-let [vents-in-point (get-in vents-map-acc [:vents point])]
              (cond-> vents-map-acc
                (= 1 vents-in-point)
                (update :dangerous-spots inc)

                :always
                (update-in [:vents point] inc))
              (assoc-in vents-map-acc
                        [:vents point]
                        1)))
          vents-map
          (points-in-segment segment)))

(defn part1-solution
  []
  (let [path "2021/day5.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             (map parse-line)
             (filter (some-fn horizontal-segment?
                              vertical-segment?))
             (reduce track-segment empty-vents-map)
             :dangerous-spots)))))

(defn part2-solution
  []
  (let [path "2021/day5.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             (map parse-line)
             (reduce track-segment empty-vents-map)
             :dangerous-spots)))))

(comment
  (part1-solution)
  (part2-solution)

  *e)
