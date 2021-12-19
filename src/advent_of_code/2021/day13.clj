(ns advent-of-code.2021.day13
  "Transparent origami
   
   This puzzle can be solved by leveraging Clojure's sorted sets.
   I'm going to keep a set of pairs representing the dots' coordinates.
   Then use subseq/rsubseq to traverse the dot coordinates that need
   to be translated into the new fold's coordinates.

   I didn't really like the solution for folding over the y-axis. I had
   to rebuild a sorted-set every time I needed to fold over the y axis, which
   is not optimal for extremely large inputs. It was still very fun to implement.

   I'd like to diminish code duplication between the folding functions,
   but I am unsure about the right concept that would be a function with the
   'common factor' between the two functions. Thus I preferred keeping them separate.
   "
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(defn parse-fold-input
  [fold-line]
  (let [[_ direction line-number] (re-find #"fold along (x|y)=(\d+)" fold-line)]
    [direction (utils/parse-int line-number)]))

(defn parse-input
  [lines]
  (let [[coordinates _ folds] (partition-by empty? lines)]
    {:coordinates (->> coordinates
                       (map #(into [] (utils/parse-integers-line % #",")))
                       (into (sorted-set)))
     :folds       (map parse-fold-input folds)}))

(defn fold-along-x-axis
  [x-to-fold coordinates]
  (let [biggest-x-coordinate (-> coordinates rseq first first)
        above-fold-x-offset (max 0 (- biggest-x-coordinate (* 2 x-to-fold)))
        below-fold-subtraction-factor (max x-to-fold
                                           (- biggest-x-coordinate x-to-fold))
        dots-above-fold (into (sorted-set)
                              (map (fn [[x y]]
                                     [(+ x above-fold-x-offset) y]))
                              (subseq coordinates < [x-to-fold 0]))]
    (into dots-above-fold
          (map (fn [[x y]]
                 [(- below-fold-subtraction-factor
                     (- x x-to-fold))
                  y]))
          (subseq coordinates >= [(inc x-to-fold) 0]))))

(defn sorted-set-by-y-axis
  []
  (sorted-set-by (fn [[x1 y1] [x2 y2]]
                   (compare [y1 x1] [y2 x2]))))

(defn fold-along-y-axis
  [y-to-fold coordinates]
  (let [coordinates (into (sorted-set-by-y-axis)
                          coordinates)
        biggest-y-coordinate (-> coordinates rseq first second)
        above-fold-y-offset (max 0 (- biggest-y-coordinate (* 2 y-to-fold)))
        below-fold-subtraction-factor (max y-to-fold
                                           (- biggest-y-coordinate y-to-fold))
        dots-above-fold (into (sorted-set)
                              (map (fn [[x y]]
                                     [x (+ y above-fold-y-offset)]))
                              (subseq coordinates < [0 y-to-fold]))]
    (into dots-above-fold
          (map (fn [[x y]]
                 [x
                  (- below-fold-subtraction-factor
                     (- y y-to-fold))]))
          (subseq coordinates >= [0 (inc y-to-fold)]))))

(defn fold-results
  [{:keys [coordinates folds]}]
  (reductions (fn [coordinates [fold-direction line-to-fold]]
                (let [fold-fn (case fold-direction
                                "x" fold-along-x-axis
                                "y" fold-along-y-axis)]
                  (fold-fn line-to-fold coordinates)))
              coordinates
              folds))

(defn part1-solution
  []
  (utils/with-input
    (fn [lines]
      (-> lines
          parse-input
          fold-results
          (nth 1)
          count))))

(defn dots-string
  [coordinates]
  (let [max-x (-> #break coordinates rseq first first)
        max-y (->> coordinates (apply max-key second) second)]
    (string/join "\n"
                 (for [y (range (inc max-y))]
                   (apply str
                          (for [x (range (inc max-x))]
                            (if (contains? coordinates [x y])
                              "#"
                              ".")))))))

(defn part2-solution
  []
  (utils/with-input
    (fn [lines]
      (let [final-dot-coordinates (-> lines
                                      parse-input
                                      fold-results
                                      last)]
        (dots-string final-dot-coordinates)))))

(comment
  (time (part1-solution))
  (time (println (part2-solution)))

  *e)
