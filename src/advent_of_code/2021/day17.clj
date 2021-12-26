(ns advent-of-code.2021.day17
  "Trick Shot
   
   The solution for this day was pretty much brute force
   with small optimizations for reducing search windows."
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(defn parse-input
  [[line]]
  (let [[_ start-x end-x start-y end-y] (re-find #"target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)"
                                               line)]
    {:x [(utils/parse-int start-x) (utils/parse-int end-x)]
     :y [(utils/parse-int start-y) (utils/parse-int end-y)]}))

(defn x-speeds
  [initial-vx]
  (lazy-cat (if (pos? initial-vx)
              (range initial-vx 0 -1)
              (range initial-vx 0 1))
            (repeat 0)))

(defn y-speeds
  [initial-vy]
  (iterate dec initial-vy))

(defn x-positions
  [initial-vx]
  (reductions + 0 (x-speeds initial-vx)))

(defn y-positions
  [initial-vy]
  (reductions + 0 (y-speeds initial-vy)))

(defn not-past-target?
  [[start end] pos]
  (if (neg? end)
    (>= pos start)
    (<= pos end)))

(defn inside-target?
  [[start end] pos]
  (<= start pos end))

(defn farthest-reachable-point
  "Only works for x speeds and positive y speeds"
  [speed]
  (let [absolute-speed (numbers/abs speed)
        absolute-value (/ (* absolute-speed
                             (inc absolute-speed))
                          2)]
    (if (neg? speed)
      (- absolute-value)
      absolute-value)))

(defn smallest-possible-x-speed
  [[start :as target]]
  (if (<= start 0)
    start
    (->> (range)
         (drop-while (comp #(< % start)
                           farthest-reachable-point))
         first)))

(defn largest-possible-x-speed
  [[_ end :as target]]
  (if (>= end 0)
    end
    (->> (range)
         (map -)
         (drop-while (comp #(> % end)
                           farthest-reachable-point))
         first)))

(defn smallest-possible-y-speed
  [[start end :as target]]
  (if (<= start 0)
    start
    (->> (range)
         (drop-while (comp #(< % start)
                           farthest-reachable-point))
         first)))

(defn largest-possible-y-speed
  [[start end :as target]]
  (if (>= end 0)
    (max end (dec (numbers/abs start)))
    (dec (numbers/abs start))))

(defn reaches-target-area?
  [[x-speed y-speed] {target-x :x target-y :y}]
  (->> (utils/zip (x-positions x-speed)
                  (y-positions y-speed))
       (take-while (fn [[x y]]
                     (and (not-past-target? target-x x)
                          (not-past-target? target-y y))))
       (utils/find-first (fn [[x y]]
                           (and (inside-target? target-x x)
                                (inside-target? target-y y))))
       some?))

(defn all-viable-initial-speeds
  [{:keys [x y] :as targets}]
  (->> (for [x-speed (range (smallest-possible-x-speed x)
                            (inc (largest-possible-x-speed x)))
             y-speed (range (smallest-possible-y-speed y)
                            (inc (largest-possible-y-speed y)))]
         [x-speed y-speed])
       (filter #(reaches-target-area? % targets))))

(defn part1-solution
  []
  (utils/with-input
    (fn [lines]
      (let [targets (parse-input lines)]
        (->> targets
             all-viable-initial-speeds
             (apply max-key second)
             second
             (farthest-reachable-point))))))

(defn part2-solution
  []
  (utils/with-input
    (fn [lines]
      (let [targets (parse-input lines)]
        (-> targets
            all-viable-initial-speeds
            count)))))

(comment
  (time (part1-solution))
  (time (part2-solution))

  *e)
