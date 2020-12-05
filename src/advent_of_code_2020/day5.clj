(ns advent-of-code-2020.day5
  (:require [advent-of-code-2020.utils :as utils]))

(defn binary-partition
  [{:keys [length] :as range} direction]
  (cond-> range
    (= :up direction)
    (update :start + (/ length 2))
    
    :always
    (update :length / 2)))

(defn partition-range->position
  [{:keys [start]}]
  start)

(defn parse-line
  [line]
  [(subs line 0 7)
   (subs line 7)])

(def base-row-range {:start 0 :length 128})
(def row-command-map {\F :down \B :up})
(def base-column-range {:start 0 :length 8})
(def column-command-map {\L :down \R :up})

(defn find-position-in-dimension
  [coordinates
   dimension-command-map
   base-range]
  (->> coordinates
       (map dimension-command-map)
       (reduce binary-partition base-range)
       partition-range->position))

(defn find-row
  [coordinates]
  (find-position-in-dimension coordinates row-command-map base-row-range))

(defn find-column
  [coordinates]
  (find-position-in-dimension coordinates column-command-map base-column-range))

(defn find-position
  [row-coordinates column-coordinates]
  (let [row (find-row row-coordinates)
        column (find-column column-coordinates)]
    [row column]))

(defn seat-id
  [row column]
  (+ column (* row 8)))

(defn part1-solution
  [lines]
  (->> lines
       (map parse-line)
       (map (partial apply find-position))
       (map (partial apply seat-id))
       (apply max)))

(defn day-solution
  []
  (utils/with-lines "resources/day5.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))
      #_(utils/tap (part2-solution lines))))
  nil)

(comment
  (binary-partition {:start 0 :length 128} :down)
  (let [test-input "FBFBBFF"
        command-map {\F :down \B :up}]
    (->> test-input
         (map command-map)
         (reduce (fn [range command]
                   (utils/tap (binary-partition range command)))
                 {:start 0 :length 128})))
  (day-solution))