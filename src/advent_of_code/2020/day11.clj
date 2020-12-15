(ns advent-of-code.2020.day11
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]))

(defn coordinates->position
  [matrix [row col]]
  (+ (* (:columns matrix) row)
     col))

(defn matrix-get
  [{:keys [data] :as matrix} position]
  (nth data (coordinates->position matrix position)))

(defn matrix-set
  [matrix position value]
  (let [position-to-set (coordinates->position matrix position)]
    (update matrix :data assoc position-to-set value)))

(defn print-matrix
  [{:keys [data columns]}]
  (doseq [row-content (->> data (partition-all columns))]
    (println (format "| %s |" (str/join " " row-content)))))

(defn valid-position?
  [{:keys [rows columns]} [row col]]
  (and (>= row 0)
       (< row rows)
       (>= col 0)
       (< col columns)))

(defn adjacent-positions
  [matrix [row col]]
  (let [possible-adjacent-deltas [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
    (->> possible-adjacent-deltas
         (map (fn [[delta-row delta-col]]
                [(+ delta-row row) (+ delta-col col)]))
         (filter (partial valid-position? matrix)))))

(defn parse-matrix
  [lines]
  (let [cols (count (first lines))
        rows (count lines)]
    {:data    (reduce into [] lines)
     :rows    rows
     :columns cols}))

(def floor-tile \.)
(defn floor-tile?
  [tile-content]
  (= floor-tile tile-content))

(def empty-seat \L)
(defn empty-seat?
  [tile-content]
  (= empty-seat tile-content))

(def occupied-seat \#)
(defn occupied-seat?
  [tile-content]
  (= occupied-seat tile-content))

(defn count-chars
  [char coll]
  (reduce (fn [acc ch]
            (if (= ch char)
              (inc acc)
              acc))
          0
          coll))

(defn next-tile-state
  [{:keys [positions-to-consider occupancy-limit]} matrix position]
  (let [current-tile-state (matrix-get matrix position)
        nearby-occupied-seats (->> (positions-to-consider matrix position)
                                   (count-chars occupied-seat))]
    (cond
      (and (empty-seat? current-tile-state)
           (zero? nearby-occupied-seats))
      occupied-seat

      (and (occupied-seat? current-tile-state)
           (>= nearby-occupied-seats occupancy-limit))
      empty-seat

      :else
      current-tile-state)))

(defn next-matrix-state
  [next-tile-state {:keys [rows columns] :as matrix}]
  (let [all-positions (for [i (range rows) j (range columns)]
                        [i j])]
    (assoc matrix :data (mapv (partial next-tile-state matrix) all-positions))))

(defn stable-matrix-state
  [next-tile-state matrix]
  (->> (range)
       (drop 1)
       (reductions (fn [[_ matrix] n]
                     (let [next-state (next-matrix-state next-tile-state matrix)]
                       (if (= (:data matrix) (:data next-state))
                         (reduced [n next-state])
                         [n next-state])))
                   [0 matrix])
       last))

(defn adjacent-positions-to-consider
  [matrix position]
  (map (partial matrix-get matrix) (adjacent-positions matrix position)))

(defn count-occupied-seats-in-stable-state
  [next-tile-state matrix]
  (let [[_ stable-state] (stable-matrix-state next-tile-state matrix)]
    (->> stable-state
         :data
         (count-chars occupied-seat))))

(defn part1-solution
  [matrix]
  (count-occupied-seats-in-stable-state
   (partial next-tile-state
            {:positions-to-consider adjacent-positions-to-consider
             :occupancy-limit       4})
   matrix))

(defn first-visible-seat
  [matrix [position-row position-col] [delta-row delta-col]]
  (loop [[row col :as position] [(+ position-row delta-row) (+ position-col delta-col)]]
    (if (valid-position? matrix position)
      (or (#{occupied-seat empty-seat} (matrix-get matrix position))
          (recur [(+ row delta-row) (+ col delta-col)]))
      nil)))

(defn visible-seats-to-consider
  [matrix position]
  (let [directions [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
    (->> directions
         (map (partial first-visible-seat matrix position))
         (filter some?))))

(defn part2-solution
  [matrix]
  (count-occupied-seats-in-stable-state
   (partial next-tile-state {:positions-to-consider visible-seats-to-consider
                             :occupancy-limit       5})
   matrix))

(defn day-solution
  []
  (utils/with-lines "2020/day11.txt"
    (fn [lines]
      (let [matrix (parse-matrix lines)]
        (utils/tap (part1-solution matrix))
        (utils/tap (part2-solution matrix)))))
  nil)

(comment
  (day-solution))
