(ns advent-of-code.2021.day2
  (:require [clojure.string :as string]
            [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]))

(defn parse-line
  [line]
  (let [[command parameter] (string/split line #" ")]
    [(keyword command) (numbers/parse-int parameter)]))

(defn move-forward
  [coordinates distance]
  (update coordinates :x + distance))

(defn move-down
  [coordinates distance]
  (update coordinates :y + distance))

(defn move-up
  [coordinates distance]
  (update coordinates :y - distance))

(def command->movement-fn
  {:forward move-forward
   :down move-down
   :up move-up})

(defn part1-solution
  []
  (let [path "2021/day2.txt"]
    (utils/with-lines path
      (fn [lines]
        (let [{:keys [x y]} (->> lines
                                 (map parse-line)
                                 (reduce (fn [coordinates [command distance]]
                                           ((command->movement-fn command) coordinates
                                                                           distance))
                                         {:x 0 :y 0}))]
          (* x y))))))

(defn move-forward-with-aim
  [{:keys [aim] :as coordinates} distance]
  (-> coordinates
      (update :x + distance)
      (update :y + (* distance aim))))

(defn move-aim-down
  [coordinates distance]
  (update coordinates :aim + distance))

(defn move-aim-up
  [coordinates distance]
  (update coordinates :aim - distance))

(def command->movement-fn-with-aim
  {:forward move-forward-with-aim
   :down move-aim-down
   :up move-aim-up})

(defn part2-solution
  []
  (let [path "2021/day2.txt"]
    (utils/with-lines path
      (fn [lines]
        (let [{:keys [x y]} (->> lines
                                 (map parse-line)
                                 (reduce (fn [coordinates [command distance]]
                                           ((command->movement-fn-with-aim command) coordinates
                                                                                    distance))
                                         {:x 0 :y 0 :aim 0}))]
          (* x y))))))

(comment
  (part1-solution)
  (part2-solution)

  *e)