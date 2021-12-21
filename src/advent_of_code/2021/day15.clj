(ns advent-of-code.2021.day15
  "Chitons
   
   I'm thinking about taking an approach based on incrementally
   updating the minimum cost to get to a certain coordinate. we go through
   the entire matrix, and for each item we compute
   
   (+ (min (cost-of-neighbor-above) (cost-of-left-neighbor))
      (cost-of-entering-current-coordinate))
   
   For the map
   1 6 1 1 1
   1 6 1 6 1
   1 1 1 6 1
   
   The scores would be (the current iteration is marked with a > sign,
   and updates are marked with a ! sign)
   
   >0 . . . .    0 >6 . . .    0 6 >7 . .    0 6 7 >8 .
    . . . . . -> .  . . . . -> . .  . . . -> . . .  . . -> ...
    . . . . .    .  . . . .    . .  . . .    . . .  . .
   
   0 6 7 8 >9     0 6 7 8 9    0  6 7 8 9    0 6  7 8 9
   . . . .  . -> >1 . . . . -> 1 >7 . . . -> 1 7 >8 . . -> ...
   . . . .  .     . . . . .    .  . . . .    . .  . . .

   0 6 7  8  9    0 6 7 8   9      0 6 7 8  9
   1 7 8 >14 . -> 1 7 8 14 >10 ->  1 7 8 14 10 -> ...
   . . .  .  .    . . . .   .     >2 . . .  .

   0  6 7 8  9     0 6 !6 !7 !8    0 6 6  7  8    0 6 6 7   8
   1  7 8 14 10 -> 1 7 !5 14 !9 -> 1 7 5  14 9 -> 1 7 5 14  9
   2 >3 . .  .     2 3 >4 .  .     2 3 4 >10 .    2 3 4 10 >10

   It seems good enough

   ---

   The approach for part 1 is going to be based, again,
   in a sorted map used as a heuristic to avoid exploring
   paths that don't seem worth exploring.

   In the set, I'm going to keep 3-tuples containing:
   [<cost so far> <distance to end> <path so far>]
   and the elements of the tuple will be used as ordering
   criteria, respectively.

   in the map

   199
   111
   991

   the sequence of sets would be

   #{[1 3 [[1 0]]], [9 3 [[0 1]]]}
   #{[2 2 [[1 0] [1 1]]], [9 3 [0 1]], [10 2 [1 0] [2 0]]}
   #{[3 1 [[1 0] [1 1] [1 2]]], [9 3 [0 1]], [10 1 [[1 0] [1 1] [2 1]]], [10 2 [[1 0] [2 0]]], [10 3 [[1 0] [1 1] [0 1]]]}
   #{[4 0 [[1 0] [1 1] [1 2] [2 2]]]}
"
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(comment
  (apply min-key
         (comp first second)
         {[1 0] [1 [[0 0]]]
          [0 1] [9 [[0 0]]]})
  )

(defn parse-input
  [lines]
  (mapv #(mapv numbers/char->int %) lines))

(defn map-height
  [map]
  (count map))

(defn map-width
  [map]
  (count (first map)))

(defn map-goal
  [map]
  [(dec (map-height map))
   (dec (map-width map))])

(defn map-size
  [map]
  (* (map-height map)
     (map-width map)))

(defn neighbor-positions
  [map [x-pos y-pos]]
  (for [[delta-x delta-y] [[-1 0] [0 1] [1 0] [0 -1]]
        :let [x (+ x-pos delta-x)
              y (+ y-pos delta-y)]
        :when (and (>= x 0)
                   (< x (map-height map))
                   (>= y 0)
                   (< y (map-width map)))]
    [x y]))

(defn compute-distances
  [cave-map]
  (let [shortest-known-path (fn shortest-known-path*
                              [to-visit]
                              (apply min-key
                                     (comp first second)
                                     to-visit))]
    (->> (iterate (fn [{:keys [distances to-visit]}]
                    (let [[position [distance path]] (shortest-known-path to-visit)
                          neighbors (->> (neighbor-positions cave-map position)
                                         (remove #(contains? distances %)))]
                      {:distances (assoc distances position [distance path])
                       :to-visit  (reduce (fn [to-visit neighbor-position]
                                            (let [new-distance     (+ distance
                                                                      (get-in cave-map neighbor-position))
                                                  [old-distance _] (get to-visit neighbor-position)]
                                              (cond
                                                #break (contains? to-visit neighbor-position)
                                                (if (< new-distance old-distance)
                                                  (assoc to-visit neighbor-position [new-distance (conj path position)])
                                                  to-visit)

                                                :else
                                                (assoc to-visit neighbor-position [new-distance (conj path position)]))))
                                          (dissoc to-visit position)
                                          neighbors)}))
                  {:distances {}
                   :to-visit {[0 0] [0 []]}})
         (map :distances)
         (utils/take-until #(= (count %) (map-size cave-map))))))

(defn part1-solution
  []
  (utils/with-input
    (fn [lines]
      (let [cave-map (parse-input lines)
            goal (map-goal cave-map)
            distances-map (->> cave-map
                               compute-distances
                               (utils/find-first #(contains? % goal)))]
        (-> distances-map
            (get goal)
            first)))))

(defn part2-solution
  []
  (utils/with-input
    (fn [lines]
      )))

(comment
  (time (part1-solution))
  (time (part2-solution))

  *e)
