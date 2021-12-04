(ns advent-of-code.2021.day3
  (:require [clojure.string :as string]
            [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]))

(defn bit-frequencies-across-sequences-by-index
  [bit-sequences index]
  (->> bit-sequences
       (map #(nth % index))
       frequencies
       (merge {\0 0, \1 0})))

(defn most-frequent-bit
  ([bit-count-for-index]
   (let [ones (get bit-count-for-index \1)
         zeroes (get bit-count-for-index \0)]
     (cond
       (> ones zeroes)
       \1

       (> zeroes ones)
       \0

       :else
       nil)))
  ([bit-count-for-index prefer-if-equal]
   (or (most-frequent-bit bit-count-for-index)
       prefer-if-equal)))

(defn least-frequent-bit
  ([bit-count-for-index]
   (let [ones (get bit-count-for-index \1)
         zeroes (get bit-count-for-index \0)]
     (cond
       (< ones zeroes)
       \1

       (< zeroes ones)
       \0

       :else
       nil)))
  ([bit-count-for-index prefer-if-equal]
   (or (least-frequent-bit bit-count-for-index)
       prefer-if-equal)))

(defn gamma-rate
  [bit-frequencies-by-index]
  (numbers/parse-int (apply str
                            (map #(-> bit-frequencies-by-index (get %) most-frequent-bit)
                                 (range (count bit-frequencies-by-index))))
                     2))

(defn epsilon-rate
  [bit-frequencies-by-index]
  (numbers/parse-int (apply str
                            (map #(-> bit-frequencies-by-index (get %) least-frequent-bit)
                                 (range (count bit-frequencies-by-index))))
                     2))

(defn part1-solution
  []
  (let [path "2021/day3.txt"]
    (utils/with-lines path
      (fn [lines]
        (let [bit-length (count (first lines))
              aggregated-bit-frequencies-by-index (reduce (fn [count-map index]
                                                            (assoc count-map
                                                                   index
                                                                   (bit-frequencies-across-sequences-by-index lines index)))
                                                          {}
                                                          (range bit-length))]
          (* (gamma-rate aggregated-bit-frequencies-by-index)
             (epsilon-rate aggregated-bit-frequencies-by-index)))))))

(defn pick-sequence-by-filtering-criteria
  [filtering-fn bit-sequences]
  (let [[[_ result]] (->> (iterate (fn [[index sequences]]
                                     [(inc index) (filtering-fn index sequences)])
                                   [0 bit-sequences])
                          (drop-while (comp (complement utils/singleton?)
                                            second)))]
    (first result)))

(defn oxygen-generator-rating
  [bit-sequences]
  (let [binary-result (pick-sequence-by-filtering-criteria
                       (fn [index sequences]
                         (let [bit-count (bit-frequencies-across-sequences-by-index sequences index)
                               desired-bit (most-frequent-bit bit-count \1)]
                           (filter #(= desired-bit
                                       (nth % index))
                                   sequences)))
                       bit-sequences)]
    (numbers/parse-int binary-result 2)))

(defn co2-scrubber-rating
  [bit-sequences]
  (let [binary-result (pick-sequence-by-filtering-criteria
                       (fn [index sequences]
                         (let [bit-count (bit-frequencies-across-sequences-by-index sequences index)
                               desired-bit (least-frequent-bit bit-count \0)]
                           (filter #(= desired-bit
                                       (nth % index))
                                   sequences)))
                       bit-sequences)]
    (numbers/parse-int binary-result 2)))

(defn part2-solution
  []
  (let [path "2021/day3.txt"]
    (utils/with-lines path
      (fn [lines]
        (* (oxygen-generator-rating lines)
           (co2-scrubber-rating lines))))))

(comment
  (part1-solution)
  (part2-solution)

  *e)