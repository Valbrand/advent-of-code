(ns advent-of-code-2020.day7
  (:require [advent-of-code-2020.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(def rule-pattern
  #"^(.+) bags contain (.+)\.$")
(def content-item-pattern
  #"(no other|(\d+) (.+)) bags?")

(defn parse-bag-contents-rule
  [container-color rule]
  (->> (str/split rule #", ")
       (map (fn [rule]
              (let [[_ rule-type quantity color] (re-find content-item-pattern rule)]
                (when (not= "no other" rule-type)
                  {color           {:contained-by {container-color (utils/parse-int quantity)}}
                   container-color {:contains {color (utils/parse-int quantity)}}}))))
       (keep identity)))

(defn merge-rules
  [a b]
  {:contained-by (merge (:contained-by a) (:contained-by b))
   :contains     (merge (:contains a) (:contains b))})

(defn parse-line
  [line]
  (let [[_ color contents] (re-find rule-pattern line)]
    (apply merge-with
           merge-rules
           {color {:color        color
                   :contains     {}
                   :contained-by {}}}
           (parse-bag-contents-rule color contents))))

(defn find-all-containers
  [starting-color rules]
  (loop [traversed-colors #{}
         [color & rest :as to-traverse] [starting-color]]
    (cond
      (empty? to-traverse)
      (disj traversed-colors starting-color)
      
      :else
      (let [color-containers (->> (get rules color)
                                  :contained-by
                                  (map first)
                                  set)]
        (recur
         (conj traversed-colors color)
         (into rest (set/difference color-containers traversed-colors)))))))

(defn part1-solution
  [lines]
  (->> lines
       (map parse-line)
       (apply merge-with merge-rules)
       (find-all-containers "shiny gold")
       count))

(defn all-contained-bags
  [starting-color rules]
  (loop [traversed-colors #{}
         contained-bags {starting-color 1}
         [[color quantity] & rest :as to-traverse] [[starting-color 1]]]
    (cond
      (empty? to-traverse)
      (dissoc contained-bags starting-color)

      :else
      (let [contained (:contains (get rules color))
            contained-quantities (->> contained
                                      (map (juxt first
                                                 (comp #(* quantity %) second)))
                                      (into {}))]
        (recur
         (conj traversed-colors color)
         (merge-with + contained-bags contained-quantities)
         (into rest contained-quantities))))))

(defn part2-solution
  [lines]
  (->> lines
       (map parse-line)
       (apply merge-with merge-rules)
       (all-contained-bags "shiny gold")
       vals
       (reduce + 0)))

(defn day-solution
  []
  (utils/with-lines "resources/day7.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))
      (utils/tap (part2-solution lines))))
  nil)

(comment
  (utils/tap (apply + 1 (utils/tap [2])))
  (->> ["no other bags" "1 blue bag"]
       (map #(re-find #"(no other|(\d+) (.+)) bags?" %)))
  (day-solution))
