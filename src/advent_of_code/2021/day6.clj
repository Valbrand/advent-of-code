(ns advent-of-code.2021.day6
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(def empty-state
  (into [] (repeat 9 0)))

(defn parse-input
  [[line]]
  (reduce (fn [state index]
            (update state index inc))
          empty-state
          (utils/parse-integers-line line #",")))

(defn timer-state-after-days
  [index days]
  (mod (+ 7 (- index days))
       7))

(defn state-after-n-days
  [n state]
  (cond
    (zero? n)
    state
    
    (>= n 7)
    (recur (- n 7)
           (reduce (fn [acc index]
                     (let [fish-at-index (get state index)]
                       (if (<= index 6)
                         (update acc (+ 2 index) + fish-at-index)
                         (-> acc
                             (update index - fish-at-index)
                             (update (- index 7) + fish-at-index)))))
                   state
                   (range 0 9)))
    
    :else
    (reduce (fn [acc index]
              (let [fish-at-index (get state index)]
                (cond-> acc
                  :always
                  (update index - fish-at-index)
                  
                  :always
                  (update (timer-state-after-days index n)
                          +
                          fish-at-index)
                  
                  (< index n)
                  (update (+ 9 (- index n)) + fish-at-index))))
            state
            (range 0 9))))

(defn part1-solution
  []
  (let [path "2021/day6.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             parse-input
             (state-after-n-days 80)
             numbers/sum)))))

(defn part2-solution
  []
  (let [path "2021/day6.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             parse-input
             (state-after-n-days 256)
             numbers/sum)))))

(comment
  (part1-solution)
  (part2-solution)

  *e)
