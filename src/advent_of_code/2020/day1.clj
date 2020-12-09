(ns advent-of-code.2020.day1
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]))

(defn find-pair
  []
  (let [path "2020/day1.txt"
        result (utils/with-lines path
                 (fn [lines]
                   (->> lines
                        (map utils/parse-int)
                        (numbers/pair-summing 2020))))]
    (if (some? result)
      (print (apply * result))
      (print "No results"))))

(defn triad-summing
  [goal ints]
  (loop [[n & rest] ints
         singles #{}
         pairs {}]
    (cond
      (nil? n)
      nil

      (contains? pairs (- goal n))
      (-> pairs
          (get (- goal n))
          (conj n))

      :else
      (let [new-pairs-map (into pairs
                                (map (fn [single-value]
                                       [(+ single-value n) [single-value n]]))
                                singles)]
        (recur rest
               (conj singles n)
               new-pairs-map)))))

(defn find-triad
  []
  (let [path "2020/day1.txt"
        result (utils/with-lines path
                 (fn [lines]
                   (->> lines
                        (map utils/parse-int)
                        (triad-summing 2020))))]
    (if (some? result)
      (print (apply * result))
      (print "No results"))))

(defn -main
  [& [type]]
  (case type
    "pair"
    (find-pair)
    
    "triad"
    (find-triad)))

(comment
  (-main "pair")
  (-main "triad"))