(ns advent-of-code.2021.day14
  "Extended Polymerization
   
   At first, I tried a naive approach before thinking (although it was
   written in the problem statement) about the exponential growth. I used
   a list of pairs (represented by strings) and mapcat'ed them using
   a map as the computation of pair insertion.
   
   Then I realized I didn't need the actual resulting polymer. Then
   it became clear it was just a counting problem. I changed the collection
   of pairs to a map that counted the frequencies of each pair in the
   polymer. The solution went from O(2^d) to O(d*n), where d is
   total steps that need to be computed and n is the total amount
   of possible pairs."
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(defn parse-input
  [lines]
  (let [[[polymer-template] _ pair-insertion-rules] (partition-by empty? lines)]
    {:polymer-template polymer-template
     :polymer-pairs    (->> polymer-template
                            (partition 2 1)
                            (map #(apply str %))
                            frequencies)
     :pair->insertion  (->> pair-insertion-rules
                            (map (fn [rule]
                                   (let [[from to] (string/split rule #" -> ")]
                                     [from [(str (first from) to) (str to (second from))]])))
                            (into {}))}))

(defn polymer-progression
  [{:keys [pair->insertion polymer-pairs]}]
  (iterate (fn [polymer-pairs]
             (reduce (fn [result [pair pair-count]]
                       (if-let [[pair-1 pair-2] (pair->insertion pair)]
                         (-> result
                             (update pair-1 numbers/safe-add pair-count)
                             (update pair-2 numbers/safe-add pair-count))
                         result))
                     {}
                     polymer-pairs))
           polymer-pairs))

(defn polymer-pairs->elements-count
  [polymer-template polymer-pairs]
  (let [first-element (first polymer-template)
        last-element (nth polymer-template (dec (count polymer-template)))
        count-with-duplicates (reduce (fn [result [pair pair-count]]
                                        (let [[element-a element-b] pair]
                                          (-> result
                                              (update element-a numbers/safe-add pair-count)
                                              (update element-b numbers/safe-add pair-count))))
                                      {}
                                      polymer-pairs)]
    (map (fn [[element count]]
           [element
            (cond-> count
              (= first-element element) dec
              (= last-element element) dec
              :always (/ 2)
              (= first-element element) inc
              (= last-element element) inc)])
         count-with-duplicates)))

(defn most-and-least-frequent-elements
  [polymer-template polymer-pairs]
  (let [max-min-fn (juxt (partial apply max-key second)
                         (partial apply min-key second))]
    (->> polymer-pairs
         (polymer-pairs->elements-count polymer-template)
         max-min-fn
         (map second))))

(defn difference-between-most-and-least-frequent-elements
  [step {:keys [polymer-template] :as polymerization-manual}]
  (let [polymer-after-n-steps (-> polymerization-manual
                                  polymer-progression
                                  (nth step))
        [most-frequent least-frequent] (most-and-least-frequent-elements polymer-template polymer-after-n-steps)]
    (- most-frequent
       least-frequent)))

(defn part1-solution
  []
  (utils/with-input
    (fn [lines]
      (->> lines
           parse-input
           (difference-between-most-and-least-frequent-elements 10)))))

(defn part2-solution
  []
  (utils/with-input
    (fn [lines]
      (->> lines
           parse-input
           (difference-between-most-and-least-frequent-elements 40)))))

(comment
  (time (part1-solution))
  (time (part2-solution))

  *e)
