(ns advent-of-code.2021.day15
  "Chitons
   
   I had a really hard time with this one. At first I didn't
   think a lot about the problem before trying to solve it and
   didn't *actually* relate it with a shortest/minimum cost graph 
   problem, and tried a really naive approach that would 
   necessarily visit all nodes and do a lot of rework.

   Later on I noticed I could solve this with djikstra/A*, but I
   had trouble with the data structure itself. I tried sorted maps,
   sorted sets, combinations between maps and sorted sets, but it was
   still prohibitively slow.

   I ended up getting to an algorithm that works similarly to A*.
   I keep track of a collection of map positions and the known 
   best cost to reach that position, as well as a collection of 'frontier'
   nodes to be visited that are the neighbors of the nodes that were
   already visited. In each iteration, I visit the frontier node with
   the smallest cost and update both collections. As the edges of the graph
   implied by the cave-map are well structured and there are no positions
   with negative cost, we can be sure that we won't ever need to
   visit a node twice.

   So that left me with a choice: which data structure should I use?
   I had two requirements:
   1. I had to be able to discover the node with the lowest cost
   2. I only needed to keep a mapping between nodes and the lowest known cost for them

   At first, I thought about using a heap-backed map, which would make
   requirement 1 run in O(1) time complexity. However, insertions in
   my implementation of the heap-map were O(n). Since each iteration
   of the algorithm reads once and adds several times to the data
   structure, I decided to go with a simple map instead.

   I was still not satisfied with the results, so I used `taoensso.tufte` to profile
   the code related to the heap and managed to get away from the unnecessarily
   inefficient O(n) insertions. After the optimizations, `assoc` operations 
   run in O(log n) - important since there are more `assoc`s than `pop`s - and
   `pop` operations are still linear but far more efficient than before.

   My first successful run of part 2 took 200s 😅 and the current 
   implementation takes around 7s.
"
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [advent-of-code.data-structures.heap :as heap]
            [clojure.string :as string]))

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

(defn steps-distance
  [[x1 y1] [x2 y2]]
  (+ (numbers/abs (- x1 x2))
     (numbers/abs (- y1 y2))))

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
  (letfn [(to-visit-score [[cost step-distance]]
            (+ cost step-distance))
          (compare-fn [a b]
            (compare (to-visit-score a) (to-visit-score b)))
          (build-to-visit [to-visit-map]
            (into (heap/heap-map-by compare-fn) to-visit-map))
          (next-tile-to-visit [to-visit]
            (peek to-visit))
          (pop-to-visit [to-visit _]
            (pop to-visit))
          (include-in-to-visit? [to-visit position cost]
            (or (not (contains? to-visit position))
                (< cost (first (get to-visit position)))))
          (conj-to-visit [to-visit position cost tiles-distance]
            (assoc to-visit position [cost tiles-distance]))]
    (let [goal (map-goal cave-map)
          initial-to-visit-map {[0 0] [0
                                       (steps-distance [0 0] goal)]}]
      (->> (iterate (fn [{:keys [distances to-visit]}]
                      (let [[position [cost-so-far _ path]] (next-tile-to-visit to-visit)
                            new-distances (assoc distances position cost-so-far)
                            new-to-visit (->> (neighbor-positions cave-map position)
                                              (remove #(contains? distances %))
                                              (reduce (fn reducer [to-visit neighbor-pos]
                                                        (let [cost-to-neighbor (+ (get-in cave-map neighbor-pos)
                                                                                  cost-so-far)]
                                                          (if (include-in-to-visit? to-visit neighbor-pos cost-to-neighbor)
                                                            (conj-to-visit to-visit
                                                                           neighbor-pos
                                                                           cost-to-neighbor
                                                                           (steps-distance neighbor-pos goal))
                                                            to-visit)))
                                                      (pop-to-visit to-visit position)))]
                        (when (contains? new-to-visit nil)
                          #break (inc 1))
                        {:distances new-distances
                         :to-visit new-to-visit}))
                    {:distances {}
                     :to-visit (build-to-visit initial-to-visit-map)})
           (map :distances)
           (utils/take-until #(= (map-size cave-map)
                                 (count %)))))))

(defn part1-solution
  []
  (utils/with-input
    (fn [lines]
      (let [cave-map (parse-input lines)
            goal (map-goal cave-map)
            distances-map (->> cave-map
                               compute-distances
                               (utils/find-first #(contains? % goal)))]
        (get distances-map goal)))))

(defn expand-map
  "Expands a cave map into a 5x larger map (in
   both dimensions) with incrementing costs for
   similar positions"
  [cave-map]
  (let [next-map-row-state (fn [map-row]
                             (mapv #(inc (mod % 9)) map-row))]
    (into []
          (comp (take 5)
                (mapcat (fn [cave-map]
                          (->> cave-map
                               (mapv (fn [map-row]
                                       (->> map-row
                                            (iterate next-map-row-state)
                                            (take 5)
                                            flatten
                                            (into []))))))))
          (iterate (fn [cave-map]
                     (mapv next-map-row-state cave-map))
                   cave-map))))

(defn part2-solution
  []
  (utils/with-input
    (fn [lines]
      (let [cave-map (expand-map (parse-input lines))
            goal (map-goal cave-map)
            distances-map (->> cave-map
                               compute-distances
                               (utils/find-first #(contains? % goal)))]
        (get distances-map goal)))))

(comment
  (time (part1-solution))
  (time (part2-solution))

  (clojure.pprint/pprint (part1-solution))

  *e)
