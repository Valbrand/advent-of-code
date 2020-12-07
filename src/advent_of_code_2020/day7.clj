(ns advent-of-code-2020.day7
  (:require [advent-of-code-2020.utils :as utils]
            [clojure.string :as str]))

(def rule-pattern
  #"^(.+) bags contain (.+)\.$")
(def content-item-pattern
  #"(no other|(\d+) (.+)) bags?")

(defn parse-bag-contents-rule
  [rule]
  (->> (str/split rule #", ")
       (map (fn [rule]
              (let [[_ rule-type quantity color] (re-find content-item-pattern rule)]
                (when (not= "no other" rule-type)
                  {:quantity quantity
                   :color    color}))))
       (keep identity)))

(defn parse-line
  [line]
  (let [[_ color contents] (re-find rule-pattern line)]
    {:color color
     :contents (parse-bag-contents-rule contents)}))

(defn part1-solution
  [lines]
  (->> lines
       (map parse-line)
       (mapv identity)))

(defn day-solution
  []
  (utils/with-lines "resources/day7.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))))
  nil)

(comment
  (->> ["no other bags" "1 blue bag"]
       (map #(re-find #"(no other|(\d+) (.+)) bags?" %)))
  (day-solution))
