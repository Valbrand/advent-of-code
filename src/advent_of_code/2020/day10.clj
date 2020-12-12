(ns advent-of-code.2020.day10
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]))

(defn joltage-differences
  [joltages]
  (let [difference-map (->> joltages
                            sort
                            (reductions (fn [[_ previous-n] n]
                                          [(- n previous-n) n])
                                        [0 0])
                            (drop 1)
                            (map first))]
    (conj difference-map 3N)))

(defn part1-solution
  [lines]
  (let [jolt-differences (->> lines
                              (map utils/parse-int)
                              joltage-differences
                              frequencies)]
    (* (get jolt-differences 1N)
       (get jolt-differences 3N))))

(def ^{:arglists '([group-lengths seq-length])}
  possible-arrangements
  (letfn [(possible-arrangements*
            [group-lengths seq-length]
            (if (< seq-length (apply min group-lengths))
              1
              (->> group-lengths
                   ;; For each of possible group length `k` in `group-lengths`, we can have a group
                   ;; that starts at any index from 0 to `(- seq-length k)`.
                   ;; For example, with each dot representing an adapter (with a `seq-length` of 4):
                   ;; - (. .) . .
                   ;; - . (. .) .
                   ;; - . . (. .)
                   (map #(range 0 (inc (- seq-length %))))
                   flatten
                   ;; The recursion is used here to compute the possible arrangements for the
                   ;; adapters "to the right" of the formed group. In the first example above,
                   ;; we can have both "(. .) (. .)" and "(. .) . ."
                   (map #(possible-arrangements group-lengths %))
                   flatten
                   numbers/sum
                   ;; The increment takes into account the possibility 
                   ;; of not grouping any adapters at all. In the `(= seq-length 4)` example above,
                   ;; the increment accounts for the scenario ". . . ."
                   inc)))]
    (memoize possible-arrangements*)))

(defn part2-solution
  [lines]
  (let [jolt-differences (->> lines
                              (map utils/parse-int)
                              joltage-differences)]
    (->> jolt-differences
         ;; group by joltage difference
         (partition-by identity)
         ;; 3-difference adapters can never be removed
         (remove #(some #{3} %))
         ;; every N-group of adapters that configure a 1 joltage difference
         ;; can be partitioned into several possible combinations of two and three adapters.
         ;; The partitions represent groups of K adapters in which K - 1 of them can be removed.
         ;; The amount of possible adapter arrangements depend only on the size of the group.
         (map count)
         (map (partial possible-arrangements [2 3]))
         ;; Now every group must be multiplied so we know the full cardinality 
         ;; of the possible combinations
         numbers/product)))

(defn day-solution
  []
  (utils/with-lines "2020/day10.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))
      (utils/tap (part2-solution lines))))
  nil)

(comment
  (day-solution))