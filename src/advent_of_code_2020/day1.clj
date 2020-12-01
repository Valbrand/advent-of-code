(ns advent-of-code-2020.day1
  (:require [clojure.java.io :as java.io]))

(defn read-input-lines
  [path]
  (with-open [rdr (java.io/reader path)]
    (line-seq rdr)))

(defn pair-summing
  [goal ints]
  (loop [[n & rest] ints
         numbers-we-need #{}]
    (cond
      (nil? n)
      nil

      (contains? numbers-we-need n)
      [n (- goal n)]

      :else
      (recur rest (conj numbers-we-need (- goal n))))))

(defn -main
  [& args]
  (let [path "resources/day1.txt"
        result (with-open [rdr (java.io/reader path)]
                 (->> rdr
                      line-seq
                      (map #(Integer/parseInt %))
                      (pair-summing 2020)))]
    (if (some? result)
      (print (apply * result))
      (print "No results"))))