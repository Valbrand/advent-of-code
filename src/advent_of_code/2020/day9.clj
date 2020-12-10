(ns advent-of-code.2020.day9
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]))

(defn find-bug
  [window-size numbers]
  (->> numbers
       (drop window-size)
       (map-indexed vector)
       (drop-while (fn [[to-drop n]]
                     (some? (numbers/pair-summing
                             n
                             (->> numbers (drop to-drop) (take window-size))))))
       first))

(defn part1-solution
  [window-size lines]
  (let [numbers (map utils/parse-int lines)]
    (second (find-bug window-size numbers))))

(defn window-summing
  [goal numbers]
  (loop [[n & rest :as all-numbers] numbers
         window clojure.lang.PersistentQueue/EMPTY
         current-sum 0]
    (cond
      (= current-sum goal)
      (seq window)

      (nil? n)
      (if (empty? window)
        nil
        (recur nil
               (pop window)
               (- current-sum (peek window))))

      (> current-sum goal)
      (recur all-numbers
             (pop window)
             (- current-sum (peek window)))

      :else
      (recur rest
             (conj window n)
             (+ n current-sum)))))

(defn part2-solution
  [window-size lines]
  (let [numbers (map utils/parse-int lines)
        [bug-idx bug-n] (find-bug window-size numbers)
        window (->> numbers
                    (take bug-idx)
                    (window-summing bug-n))]
    (+ (apply min window)
       (apply max window))))

(defn day-solution
  []
  (utils/with-lines "2020/day9.txt"
    (fn [lines]
      (let [window-size 25]
        (utils/tap (part1-solution window-size lines))
        (utils/tap (part2-solution window-size lines)))))
  nil)

(comment
  (day-solution))