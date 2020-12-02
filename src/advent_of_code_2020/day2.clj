(ns advent-of-code-2020.day2
  (:require [advent-of-code-2020.utils :as utils]))

(def line-pattern
  #"(\d+)-(\d+) ([a-z]): (.+)")

(defn parse-line
  [raw-line]
  (let [[[_ min max [char] password :as re-line]] (re-seq line-pattern raw-line)
        policy {:char char
                :min  (Integer/parseInt min)
                :max  (Integer/parseInt max)}]
    {:policy policy
     :password password}))

(defn count-chars
  [s]
  (reduce (fn [result char]
            (update result char (fnil inc 0))) 
          {}
          s))

(defn valid-password?
  [{:keys [policy password]}]
  (let [chars-count (count-chars password)
        policy-char-count (get chars-count (:char policy) 0)]
    (<= (:min policy) policy-char-count (:max policy))))

(defn part1-solution
  []
  (let [path "resources/day2.txt"]
    (utils/with-lines path
      (->> lines
           (map parse-line)
           (filter valid-password?)
           count))))

(comment
  (part1-solution))