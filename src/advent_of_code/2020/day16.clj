(ns advent-of-code.2020.day16
  (:require [advent-of-code.numbers :as numbers]
            [advent-of-code.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(def field-rule-pattern #"(.+?): (\d+-\d+)(?: or (\d+-\d+))+")
(defn parse-field-rules
  [lines]
  (reduce (fn [rules line]
            (let [[_ field-name & rule-intervals] (re-find field-rule-pattern line)
                  intervals (->> rule-intervals
                                 (map #(map utils/parse-int (str/split % #"-"))))]
              (assoc rules field-name intervals)))
          {}
          lines))

(defn parse-tickets
  [lines]
  (reduce (fn [tickets line]
            (into tickets [(map utils/parse-int (str/split line #","))]))
          []
          lines))

(defn parse-notes
  [lines]
  (let [[field-lines
         _ [_ & your-ticket-lines]
         _ [_ & nearby-tickets-lines]] (partition-by empty? lines)
        field-rules (parse-field-rules field-lines)
        [your-ticket] (parse-tickets your-ticket-lines)
        nearby-tickets (parse-tickets nearby-tickets-lines)]
    {:field-rules    field-rules
     :your-ticket    your-ticket
     :nearby-tickets nearby-tickets}))

(defn outside-interval?
  [[lower-bound upper-bound] value]
  (or (< value lower-bound)
      (> value upper-bound)))
(def inside-interval? (complement outside-interval?))

(defn nonsense-fields
  [field-rules ticket]
  (let [intervals (->> field-rules (map second) utils/lazy-cat*)]
    (->> ticket
         (filter #(every? (fn [interval]
                            (outside-interval? interval %))
                          intervals)))))

(defn all-nonsense-fields
  [field-rules tickets]
  (->> tickets
       (map (partial nonsense-fields field-rules))
       flatten))

(defn part1-solution
  [{:keys [field-rules nearby-tickets]}]
  (numbers/sum (all-nonsense-fields field-rules nearby-tickets)))

(defn possible-fields-for-value
  [field-rules value]
  (->> field-rules
       (filter (fn [[field-name intervals]]
                 (some #(inside-interval? % value) intervals)))
       (map key)))

(defn invalid-ticket?
  [field-rules ticket]
  (seq (nonsense-fields field-rules ticket)))

(defn narrow-down-field-hypotheses
  [field-hipotheses]
  (let [narrowed-down-previously (->> field-hipotheses
                                      (filter utils/singleton?)
                                      (reduce into #{}))
        adjusted-hypotheses (map #(if (utils/singleton? %)
                                    %
                                    (set/difference % narrowed-down-previously))
                                 field-hipotheses)]
    (if (every? utils/singleton? adjusted-hypotheses)
      adjusted-hypotheses
      (recur adjusted-hypotheses))))

(defn discover-field-positions
  [field-rules tickets]
  (->> tickets
       (map (fn [ticket]
              (map #(set (possible-fields-for-value field-rules %)) ticket)))
       (apply map set/intersection)
       narrow-down-field-hypotheses
       (map first)))

(defn part2-solution
  [{:keys [field-rules your-ticket nearby-tickets]}]
  (let [field-positions (->> nearby-tickets
                             (remove (partial invalid-ticket? field-rules))
                             (discover-field-positions field-rules))
        your-ticket-fields (map vector field-positions your-ticket)]
    (->> your-ticket-fields
         (filter (comp #(str/starts-with? % "departure") first))
         (map val)
         numbers/product)))

(defn day-solution
  []
  (utils/with-lines "2020/day16.txt"
    (fn [lines]
      (let [notes (parse-notes lines)]
        #tap (part1-solution notes)
        #tap (part2-solution notes))))
  nil)

(comment
  (day-solution))