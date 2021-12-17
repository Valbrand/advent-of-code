(ns advent-of-code.2021.day9
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(defn parse-input
  [lines]
  (mapv #(mapv numbers/char->int %) lines))

(defn neighbors
  [depth-map [row-idx col-idx]]
  (let [row-within-map? (fn row-within-map? [row]
                          (<= 0 row (dec (count depth-map))))
        col-within-map? (fn col-within-map? [col]
                          (<= 0 col (dec (count (get depth-map row-idx)))))
        position-deltas [[-1 0] [0 1] [1 0] [0 -1]]]
    (->> #break position-deltas
         (map (fn [[row-delta col-delta]]
                [(+ row-idx row-delta) (+ col-idx col-delta)]))
         (filter (fn [[row col]]
                   (and (row-within-map? row)
                        (col-within-map? col)))))))

(defn memoized-neighbors
  [depth-map]
  (memoize (partial neighbors depth-map)))

(defn lazy-coordinates-seq
  [depth-map]
  (for [row (range (count depth-map))
        col (range (count (get depth-map row)))]
    [row col]))

(defn find-low-points
  [depth-map]
  (let [neighbors (memoized-neighbors depth-map)]
    (->> (lazy-coordinates-seq depth-map)
         (map (fn [position]
                (let [element (get-in depth-map position)]
                  (when (->> (neighbors position)
                             (map #(get-in depth-map %))
                             (every? (partial < element)))
                    position))))
         (filter some?))))

(defn part1-solution
  []
  (let [path "2021/day9.txt"]
    (utils/with-lines path
      (fn [lines]
        (let [depth-map (parse-input lines)]
          (->> depth-map
               find-low-points
               (map #(get-in depth-map %))
               (map inc)
               (reduce + 0)))))))

(defn basin-for-low-point
  [depth-map low-point]
  (let [neighbors (memoized-neighbors depth-map)]
    (->> {:to-visit (set (neighbors low-point))
          :visited #{low-point}
          :basin #{low-point}}
         (iterate (fn [{:keys [to-visit basin visited]}]
                    (let [visiting (first to-visit)
                          include-in-basin? (not= 9 (get-in depth-map visiting))
                          nodes-to-visit (if include-in-basin?
                                           (filter #(not (contains? visited %)) 
                                                   (neighbors visiting))
                                           [])]
                      {:to-visit (disj (into to-visit nodes-to-visit) 
                                        visiting)
                       :visited (conj visited visiting)
                       :basin (if include-in-basin?
                                (conj basin visiting)
                                basin)})))
         (drop-while (comp seq :to-visit))
         (map :basin)
         (take 1)
         first)))

(defn part2-solution
  []
  (let [path "2021/day9.txt"]
    (utils/with-lines path
      (fn [lines]
        (let [depth-map (parse-input lines)]
          (->> depth-map
               find-low-points
               (map (partial basin-for-low-point depth-map))
               (map count)
               (sort >)
               (take 3)
               (numbers/product)))))))



(comment
  (time (part1-solution))

  (part2-solution)

  *e)
