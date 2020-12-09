(ns advent-of-code.2015.day5
  (:require [advent-of-code.utils :as utils]))

(def forbidden-substrings-pattern #"(ab|cd|pq|xy)")
(def double-letter-pattern #"(.)\1")

(defn without-forbidden-substrings?
  [s]
  (nil? (re-find forbidden-substrings-pattern s)))

(defn contains-double-letter?
  [s]
  (some? (re-find double-letter-pattern s)))

(defn has-enough-vowels?
  [s]
  (>= (->> s
           (filter #{\a \e \i \o \u})
           count)
      3))

(defn nice?
  [string]
  (and (without-forbidden-substrings? string)
       (contains-double-letter? string)
       (has-enough-vowels? string)))

(defn part1-solution
  [lines]
  (->> lines
       (filter nice?)
       count))

(def pair-appearing-twice-pattern #"(.{2}).*\1")
(def letter-surrounded-pattern #"(.).\1")

(defn has-pair-appearing-twice?
  [s]
  (some? (re-find pair-appearing-twice-pattern s)))

(defn has-letter-surrounded-by-equal-letters?
  [s]
  (some? (re-find letter-surrounded-pattern s)))

(defn nice?'
  [string]
  (and (has-pair-appearing-twice? string)
       (has-letter-surrounded-by-equal-letters? string)))

(defn part2-solution
  [lines]
  (->> lines
       (filter nice?')
       count))

(defn day-solution
  []
  (utils/with-lines "2015/day5.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))
      (utils/tap (part2-solution lines)))))

(comment
  (has-pair-appearing-twice? "aaa")
  (day-solution))
