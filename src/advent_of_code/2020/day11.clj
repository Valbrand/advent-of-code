(ns advent-of-code.2020.day11
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]))

(def matrix {:data    ["00" "01" "02" "10" "11" "12" "20" "21" "22"]
             :rows    3
             :columns 3})

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
  [matrix position]
  (let [possible-adjacent-deltas [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
    (->> possible-adjacent-deltas
         (map #(map (comp (partial apply +) vector) % position))
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

(defn next-tile-state
  [matrix position]
  (let [current-tile-state (matrix-get matrix position)
        adjacent-position-states (delay (->> position
                                             (adjacent-positions matrix)
                                             (map #(matrix-get matrix %))
                                             frequencies))]
    (cond
      (and (empty-seat? current-tile-state)
           (zero? (get @adjacent-position-states occupied-seat 0)))
      occupied-seat

      (and (occupied-seat? current-tile-state)
           (>= (get @adjacent-position-states occupied-seat 0) 4))
      empty-seat

      :else
      current-tile-state)))

(defn next-matrix-state
  [{:keys [rows columns] :as matrix}]
  (let [all-positions (for [i (range rows) j (range columns)]
                        [i j])]
    (assoc matrix :data (mapv (partial next-tile-state matrix) all-positions))))

(defn stable-matrix-state
  [matrix]
  (->> (range)
       (drop 1)
       (reductions (fn [[_ matrix] n]
                     (let [next-state (next-matrix-state matrix)]
                       (if (= (:data matrix) (:data next-state))
                         (reduced [n next-state])
                         [n next-state])))
                   [0 matrix])
       last))

(defn part1-solution
  [matrix]
  (let [[_ stable-state] (stable-matrix-state matrix)]
    (-> stable-state
        :data
        frequencies
        (get occupied-seat 0))))

(defn day-solution
  []
  (utils/with-lines "2020/day11.txt"
    (fn [lines]
      (let [matrix (parse-matrix lines)]
        (utils/tap (part1-solution matrix)))))
  nil)

(comment
  (for [x (range -1 2) y (range -1 2) :when (or (not (zero? x)) (not (zero? y)))]
    [x y])
  (->> [1 1]
       (adjacent-positions matrix)
       (map (partial matrix-get matrix)))
  
  (print-matrix matrix)
  
  (->> [".LL" "LLL" "..."]
       parse-matrix
       next-matrix-state
       next-matrix-state)
  (day-solution))
