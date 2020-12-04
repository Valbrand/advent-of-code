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

(defn interval-valid?
  [min max]
  #(<= min (utils/parse-int %) max))

(def height-pattern
  #"(\d+)(cm|in)")

(def height-validators
  {"cm" (interval-valid? 150 193)
   "in" (interval-valid? 59 76)})

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
  {"byr" (interval-valid? 1920 2002)
   "iyr" (interval-valid? 2010 2020)
   "eyr" (interval-valid? 2020 2030)
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
    (utils/tap (part1-solution lines))
    (utils/tap (part2-solution lines)))
  nil)

(comment
  *e
  (re-find hair-color-pattern "#12312q")
  (day-solution))