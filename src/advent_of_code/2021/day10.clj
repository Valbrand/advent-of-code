(ns advent-of-code.2021.day10
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(def opening->closing
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(def opening-bracket? #{\( \[ \{ \<})

(defn analyze-line
  [line]
  (let [initial-line-state {:status :ok
                            :stack  []
                            :line   line}
        line-state-reducer (fn line-state-reducer
                             [{:keys [stack] :as state} character]
                             (cond
                               (opening-bracket? character)
                               (update state :stack conj (opening->closing character))

                               (= (peek stack) character)
                               (update state :stack pop)

                               :else
                               (reduced {:status :corrupted
                                         :violation character
                                         :line line})))]
    (->> line
         (reduce line-state-reducer initial-line-state))))

(def violation->points
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn part1-solution
  []
  (let [path "2021/day10.txt"]
    (utils/with-lines path
      (fn [lines]
        (transduce (comp (map analyze-line)
                         (filter #(= :corrupted (:status %)))
                         (map (comp violation->points :violation)))
                   +
                   0
                   lines)))))

(def closing-bracket->autocompletion-points
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn autocompletion-score-for-line
  [{:keys [stack] :as line-state}]
  (reduce (fn [score character]
            (+ (* 5 score)
               (closing-bracket->autocompletion-points character)))
          0
          (rseq stack)))

(defn autocompletion-score-for-program
  [line-states]
  (->> line-states
       (filter #(= :ok (:status %)))
       (map autocompletion-score-for-line)
       utils/median))

(defn part2-solution
  []
  (let [path "2021/day10.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             (map analyze-line)
             autocompletion-score-for-program)))))

(comment
  (time (part1-solution))
  (time (part2-solution))

  *e)
