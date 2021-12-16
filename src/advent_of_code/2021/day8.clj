(ns advent-of-code.2021.day8
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn parse-input
  [lines]
  (->> lines
       (map (fn [line]
              (let [[patterns digits] (string/split line #"\s+\|\s+")]
                {:patterns (->> (string/split patterns #"\s+")
                                (map set))
                 :digits   (->> (string/split digits #"\s+")
                                (map set))})))))

(def number->simple-digit-matcher
  {1 (fn [pattern] (= 2 (count pattern)))
   4 (fn [pattern] (= 4 (count pattern)))
   7 (fn [pattern] (= 3 (count pattern)))
   8 (fn [pattern] (= 7 (count pattern)))})

(defn part1-solution
  []
  (let [path "2021/day8.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             parse-input
             (map (fn [{:keys [digits]}]
                    (->> digits
                         (filter (some-fn (number->simple-digit-matcher 1)
                                          (number->simple-digit-matcher 4)
                                          (number->simple-digit-matcher 7)
                                          (number->simple-digit-matcher 8)))
                         count)))
             (numbers/sum))))))

(defn patterns-with-length
  [length {:keys [patterns]}]
  (filter #(= length (count %)) patterns))

(defn simple-matcher
  [n {:keys [patterns]}]
  (utils/find-first (number->simple-digit-matcher n)
                    patterns))

(declare number->pattern-matcher)
(declare segment->segment-matcher)

(def number->pattern-matcher
  (utils/map-vals
   memoize
   {0 (fn number-0-pattern-matcher
        [display]
        (let [segment-d ((segment->segment-matcher \d) display)]
          (utils/find-first (complement #(contains? % segment-d))
                            (patterns-with-length 6 display))))
    1 (partial simple-matcher 1)
    2 (fn number-2-pattern-matcher
        [display]
        (-> (patterns-with-length 5 display)
            set
            (disj ((number->pattern-matcher 5) display))
            (disj ((number->pattern-matcher 3) display))
            first))
    3 (fn number-3-pattern-matcher
        [display]
        (let [pattern-8 ((number->pattern-matcher 8) display)
              five-segment-patterns (set (patterns-with-length 5 display))]
          (utils/find-first (fn [pattern]
                              (= pattern-8
                                 (apply set/union (disj five-segment-patterns pattern))))
                            five-segment-patterns)))
    4 (partial simple-matcher 4)
    5 (fn number-5-pattern-matcher
        [display]
        (let [segment-b ((segment->segment-matcher \b) display)]
          (utils/find-first #(contains? % segment-b) 
                            (patterns-with-length 5 display))))
    6 (fn number-2-pattern-matcher
        [display]
        (-> (patterns-with-length 6 display)
            set
            (disj ((number->pattern-matcher 0) display))
            (disj ((number->pattern-matcher 9) display))
            first))
    7 (partial simple-matcher 7)
    8 (partial simple-matcher 8)
    9 (fn number-9-pattern-matcher
        [display]
        (utils/find-first (fn [pattern]
                            (empty? (set/difference pattern
                                                    ((number->pattern-matcher 1) display)
                                                    ((number->pattern-matcher 5) display))))
                          (patterns-with-length 6 display)))}))

(def segment->segment-matcher
  (utils/map-vals
   memoize
   {\a (fn segment-a-matcher
         [display]
         (first (set/difference ((number->pattern-matcher 7) display)
                                ((number->pattern-matcher 1) display))))
    \b (fn segment-b-matcher
         [display]
         (first (set/difference ((number->pattern-matcher 4) display)
                                ((number->pattern-matcher 3) display))))
    \d (fn segment-d-matcher
         [display]
         (first (disj (set/difference ((number->pattern-matcher 4) display)
                                      ((number->pattern-matcher 1) display))
                      ((segment->segment-matcher \b) display))))}))

(defn deduct-patterns
  [display]
  (assoc display
         :pattern-mappings
         (reduce (fn [pattern-mappings n]
                   (assoc pattern-mappings ((number->pattern-matcher n) display) n))
                 {}
                 (range 0 10))))

(defn compute-display-value
  [{:keys [digits pattern-mappings]}]
  (->> digits
       (map pattern-mappings)
       (reduce (fn [acc digit-value]
                 (+ (* 10 acc) digit-value))
               0)))

(defn part2-solution
  []
  (let [path "2021/day8.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             parse-input
             (map deduct-patterns)
             (map compute-display-value)
             numbers/sum)))))

(comment
  (part1-solution)
  (part2-solution)

  (def segments-needed
    {1 2
     4 4
     7 3
     8 7

     2 5
     3 5
     5 5

     0 6
     6 6
     9 6})

  *e)
