(ns advent-of-code-2020.day4
  (:require [advent-of-code-2020.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn passport-separator?
  [line-group]
  (and (= 1 (count line-group))
       (empty? (first line-group))))

(defn split-passport-lines
  [all-lines]
  (->> all-lines
       (partition-by empty?)
       (remove passport-separator?)))

(defn parse-line
  [line]
  (->> (str/split line #" ")
       (map #(str/split % #":"))
       (into {})))

(defn parse-lines
  [lines]
  (->> lines
       split-passport-lines
       (map (comp (partial apply merge)
                  (partial map parse-line)))))

(defn valid-passport?
  [required-fields optional-fields passport]
  (let [actual-required-fields (set/difference required-fields optional-fields)]
    (->> passport
         keys
         set
         (set/difference actual-required-fields)
         empty?)))

(defn part1-solution
  [lines]
  (let [required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"}
        optional-fields #{"cid"}]
    (->> lines
         parse-lines
         (filter (partial valid-passport? required-fields optional-fields))
         count)))

(defn day-solution
  []
  (utils/with-lines "resources/day4.txt"
    (utils/tap (part1-solution lines))))

(comment
  *e
  (day-solution))