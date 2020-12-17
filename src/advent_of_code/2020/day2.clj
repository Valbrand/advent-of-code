(ns advent-of-code.2020.day2
  (:require [advent-of-code.utils :as utils]))

(def line-pattern
  #"(\d+)-(\d+) ([a-z]): (.+)")

(defn parse-part1-line
  [raw-line]
  (let [[_ min max [char] password] (re-find line-pattern raw-line)
        policy {:char char
                :min  (Integer/parseInt min)
                :max  (Integer/parseInt max)}]
    {:policy   policy
     :password password}))


(defn valid-part1-password?
  [{:keys [policy password]}]
  (let [policy-char (:char policy)
        policy-char-count (->> password
                               (filter #{policy-char})
                               count)]
    (<= (:min policy) policy-char-count (:max policy))))

(defn part1-solution
  [lines]
  (->> lines
       (map parse-part1-line)
       (filter valid-part1-password?)
       count))

(defn parse-part2-line
  [raw-line]
  (let [[_ index1 index2 [char] password] (re-find line-pattern raw-line)
        policy {:char   char
                :index1 (dec (Integer/parseInt index1))
                :index2 (dec (Integer/parseInt index2))}]
    {:policy   policy
     :password password}))

(defn chars-at-policy-indices
  [policy password]
  (->> policy
       ((juxt :index1 :index2))
       (map #(nth password %))))

(defn valid-part2-password?
  [{:keys [policy password]}]
  (let [policy-char (:char policy)
        [index1-password-char index2-password-char] (chars-at-policy-indices policy password)
        index1-matches? (= policy-char index1-password-char)
        index2-matches? (= policy-char index2-password-char)]
    (not= index1-matches? index2-matches?)))

(defn part2-solution
  [lines]
  (->> lines
       (map parse-part2-line)
       (filter valid-part2-password?)
       count))

(defn day-solution
  []
  (utils/with-lines "2020/day2.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))
      (utils/tap (part2-solution lines)))))

(comment
  (day-solution))