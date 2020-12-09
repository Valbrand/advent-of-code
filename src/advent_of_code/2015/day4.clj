(ns advent-of-code.2015.day4
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str])
  (:import [java.security MessageDigest]))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn smallest-salted-md5-with-prefix
  [base prefix]
  (eduction (filter pos?)
            (map (juxt identity (comp md5 (partial str base))))
            (filter (comp #(str/starts-with? % prefix) second))
            (range)))

(defn part1-solution
  [input]
  (first (smallest-salted-md5-with-prefix input "00000")))

(defn part2-solution
  [input]
  (first (smallest-salted-md5-with-prefix input "000000")))

(defn day-solution
  []
  (let [input "ckczppom"]
    (utils/tap (part1-solution input))
    (utils/tap (part2-solution input))))

(comment
  (day-solution))