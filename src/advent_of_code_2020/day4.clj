(ns advent-of-code-2020.day4
  (:require [advent-of-code-2020.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-line
  [line]
  (->> (str/split line #" ")
       (map #(str/split % #":"))
       (into {})))

(defn parse-lines
  [lines]
  (->> lines
       utils/split-by-empty-lines
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

(defn inside-interval?
  [min max]
  #(<= min (utils/parse-int %) max))

(def height-pattern
  #"(\d+)(cm|in)")

(def height-validators
  {"cm" (inside-interval? 150 193)
   "in" (inside-interval? 59 76)})

(defn valid-height?
  [height]
  (let [[_ value-str unit] (re-find height-pattern height)
        height-validator (get height-validators unit (constantly false))]
    (height-validator value-str)))

(def hair-color-pattern #"^#[0-9a-f]{6}$")
(def passport-id-pattern #"^\d{9}$")

(defn matches-pattern?
  [pattern]
  #(boolean (re-find pattern %)))

(def valid-eye-color?
  #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(def field-validations
  {"byr" (inside-interval? 1920 2002)
   "iyr" (inside-interval? 2010 2020)
   "eyr" (inside-interval? 2020 2030)
   "hgt" valid-height?
   "hcl" (matches-pattern? hair-color-pattern)
   "ecl" valid-eye-color?
   "pid" (matches-pattern? passport-id-pattern)})

(defn passport-with-valid-fields?
  [passport]
  (reduce (fn [_ [key value]]
            (let [validator (get field-validations key (constantly true))]
              (try
                (if (validator value)
                  true
                  (reduced false))
                (catch Exception _
                  (reduced false)))))
          true
          passport))

(defn part2-solution
  [lines]
  (let [required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"}
        optional-fields #{"cid"}]
    (->> lines
         parse-lines
         (filter (partial valid-passport? required-fields optional-fields))
         (filter passport-with-valid-fields?)
         count)))

(defn day-solution
  []
  (utils/with-lines "resources/day4.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))
      (utils/tap (part2-solution lines))))
  nil)

(comment
  *e
  (re-find hair-color-pattern "#12312q")
  (day-solution))