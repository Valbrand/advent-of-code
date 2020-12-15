(ns advent-of-code.2020.day13
  (:require [advent-of-code.numbers :as numbers]
            [advent-of-code.utils :as utils]
            [clojure.string :as str]))

(defn nearest-departure
  [base-timestamp bus-id]
  (if (numbers/divisible? base-timestamp bus-id)
    base-timestamp
    (* bus-id (inc (quot base-timestamp bus-id)))))

(defn part1-solution
  [{:keys [base-timestamp bus-ids]}]
  (let [[bus-id closest-bus-departure]
        (reduce (fn [[_ departure :as result] bus-id]
                  (let [nearest-departure-for-bus (nearest-departure base-timestamp bus-id)]
                    (cond
                      (= nearest-departure-for-bus base-timestamp)
                      (reduced [bus-id base-timestamp])

                      (< nearest-departure-for-bus departure)
                      [bus-id nearest-departure-for-bus]

                      :else
                      result)))
                [-1 (* 2 base-timestamp)]
                bus-ids)]
    (* bus-id (- closest-bus-departure base-timestamp))))

(defn part2-solution ;; Chinese remainder theorem
  [{:keys [all-data]}]
  (let [buses+offsets (->> all-data
                           (map-indexed vector)
                           (remove (comp string? second)))
        bus-ids-product (->> buses+offsets (map second) numbers/product)]
    (mod (->> buses+offsets
              (map (fn [[offset bus-id]]
                     (let [product-quotient (quot bus-ids-product bus-id)
                           [_ s] (numbers/extended-gcd product-quotient bus-id)]
                       (* offset s product-quotient))))
              numbers/sum
              numbers/abs)
         bus-ids-product)))

(defn parse-notes
  [lines]
  (let [earliest-timestamp (utils/parse-int (first lines))
        buses (str/split (second lines) #",")
        bus-ids (->> buses
                     (remove (partial = "x"))
                     (map utils/parse-int))
        all-data (->> buses
                      (map #(if (re-find utils/digits-pattern %)
                              (utils/parse-int %)
                              %)))]
    {:base-timestamp earliest-timestamp
     :bus-ids        bus-ids
     :all-data       all-data}))

(defn day-solution
  []
  (utils/with-lines "2020/day13.txt"
    (fn [lines]
      (let [notes (parse-notes lines)]
        (utils/tap (part1-solution notes))
        (utils/tap (part2-solution notes)))))
  nil)

(comment
  (parse-notes ["1203" "1,2,x,3,x,x,4"])
  
  (day-solution))