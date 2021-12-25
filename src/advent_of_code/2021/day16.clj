(ns advent-of-code.2021.day16
  "Packet Decoder
   
   The solution for day 16 was a very straightforward
   implementation of the problem description. No pitfalls
   found during implementation."
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(def hex->binary
  {\0 [\0 \0 \0 \0]
   \1 [\0 \0 \0 \1]
   \2 [\0 \0 \1 \0]
   \3 [\0 \0 \1 \1]
   \4 [\0 \1 \0 \0]
   \5 [\0 \1 \0 \1]
   \6 [\0 \1 \1 \0]
   \7 [\0 \1 \1 \1]
   \8 [\1 \0 \0 \0]
   \9 [\1 \0 \0 \1]
   \A [\1 \0 \1 \0]
   \B [\1 \0 \1 \1]
   \C [\1 \1 \0 \0]
   \D [\1 \1 \0 \1]
   \E [\1 \1 \1 \0]
   \F [\1 \1 \1 \1]})

(def bit->numeric-value
  {\0 0
   \1 1})

(defn new-bit-seq
  [bits]
  #:bit-seq {:consumed 0
             :bits bits})

(defn parse-input
  [[command]]
  (new-bit-seq (flatten (map hex->binary command))))

(defn read-n-bits-error!
  [n bit-seq]
  (throw (ex-info (str "Tried to read " n " bits, only " (count bit-seq) " available")
                  {})))

(defn read-bits
  [n {:bit-seq/keys [bits consumed]}]
  (let [[value remaining-bits] (-> (iterate (fn [[result iter-bits]]
                                              (when (empty? iter-bits)
                                                (read-n-bits-error! n bits))
                                              [(+ (* 2 result)
                                                  (bit->numeric-value (first iter-bits)))
                                               (rest iter-bits)])
                                            [0 bits])
                                   (nth n))]
    [value
     #:bit-seq {:consumed (+ n consumed)
                :bits     remaining-bits}]))

(defn read-raw-bits
  [n {:bit-seq/keys [bits consumed]}]
  (let [[raw-bits remaining-bits] (-> (iterate (fn [[consumed iter-bits]]
                                                 (when (empty? iter-bits)
                                                   (read-n-bits-error! n bits))
                                                 [(conj consumed (first iter-bits))
                                                  (rest iter-bits)])
                                               [[] bits])
                                      (nth n))]
    [raw-bits
     #:bit-seq {:consumed (+ n consumed)
                :bits     remaining-bits}]))

(defn drop-bits
  [n {:bit-seq/keys [bits consumed]}]
  (let [remaining-bits (-> (iterate (fn [iter-bits]
                                      (when (empty? iter-bits)
                                        (read-n-bits-error! n bits))
                                      (rest iter-bits))
                                    bits)
                           (nth n))]
    #:bit-seq {:consumed (+ n consumed)
               :bits     remaining-bits}))

(defn decode-packet-header
  [bit-seq]
  (let [[version bit-seq] (read-bits 3 bit-seq)
        [packet-type bit-seq] (read-bits 3 bit-seq)]
    [{:packet/version version
      :packet/type packet-type}
     bit-seq]))

(defn decode-numeric-literal
  [packet bit-seq]
  (letfn [(join-4-bit-word [n bit-seq]
            (let [[from-bits bit-seq] (read-bits 4 bit-seq)]
              [(+ (* 16 n)
                  from-bits)
               bit-seq]))]
    (loop [[flag bit-seq] (read-bits 1 bit-seq)
           result 0]
      (if (zero? flag)
        (let [[result bit-seq] (join-4-bit-word result bit-seq)]
          [(assoc packet :packet/literal result)
           bit-seq])
        (let [[result bit-seq] (join-4-bit-word result bit-seq)]
          (recur (read-bits 1 bit-seq)
                 result))))))

(declare decode-packets)
(declare decode-packet)
(defn bit-length-based-operator-decoder
  [bit-seq]
  (let [[bit-length bit-seq] (read-bits 15 bit-seq)
        [sub-packet-bit-seq bit-seq] (read-raw-bits bit-length bit-seq)
        [sub-packets _] (decode-packets (new-bit-seq sub-packet-bit-seq))]
    [sub-packets
     bit-seq]))

(defn packet-count-based-operator-decoder
  [bit-seq]
  (let [[packet-count bit-seq] (read-bits 11 bit-seq)
        [sub-packets bit-seq] (decode-packets packet-count bit-seq)]
    [sub-packets
     bit-seq]))

(def length-type-id->body-decoder
  {0 bit-length-based-operator-decoder
   1 packet-count-based-operator-decoder})

(defn decode-operator-packet
  [packet bit-seq]
  (let [[length-type bit-seq] (read-bits 1 bit-seq)
        sub-packets-decoder-fn (length-type-id->body-decoder length-type)
        [sub-packets bit-seq] (sub-packets-decoder-fn bit-seq)]
    [(assoc packet
            :packet/length-type length-type
            :packet/sub-packets sub-packets)
     bit-seq]))

(defn packet-type->body-decoder
  [type]
  (if (= 4 type)
    decode-numeric-literal
    decode-operator-packet))

(defn decode-packet
  [bit-seq]
  (let [[{:packet/keys [type] :as header} bit-seq] (decode-packet-header bit-seq)
        decode-fn (packet-type->body-decoder type)
        [packet bit-seq] (decode-fn header bit-seq)]
    [packet
     bit-seq]))

(defn- decode-packets*
  [bit-seq]
  (iterate (fn [[packets iter-bit-seq]]
             (let [[packet iter-bit-seq] (decode-packet iter-bit-seq)]
               [(conj packets packet)
                iter-bit-seq]))
           [[] bit-seq]))

(defn decode-packets
  "Note: assumes the entire bit sequence is meaningful
   i.e. does not contain zeroes in the end"
  ([bit-seq]
   (->> (decode-packets* bit-seq)
        (utils/find-first (comp empty? :bit-seq/bits second))))
  ([n bit-seq]
   (-> (decode-packets* bit-seq)
       (nth n))))

(defn packet-score
  [packet]
  (letfn [(packet-score* [score packets]
            (let [packet (peek packets)]
              (cond
                (nil? packet)
                score
                
                (not (contains? packet :packet/sub-packets))
                (recur (+ score (:packet/version packet))
                       (pop packets))
                
                :else
                (recur (+ score (:packet/version packet))
                       (into (pop packets)
                             (:packet/sub-packets packet))))))]
    (packet-score* 0 [packet])))

(defn part1-solution
  []
  (utils/with-input
    (fn [lines]
      (->> lines
           parse-input
           decode-packet
           first
           packet-score))))

(declare packet-value)

(defn literal-packet-value
  [{:packet/keys [literal]}]
  literal)

(defn aggregate-sub-packets
  [aggregate-fn]
  (fn [{:packet/keys [sub-packets]}]
    (->> sub-packets
         (map packet-value)
         aggregate-fn)))

(defn pred-sub-packet-value-fn
  [pred]
  (fn [packet]
    (let [[a b] (map packet-value (:packet/sub-packets packet))]
      (if (pred a b)
        1
        0))))

(def packet-type->value-fn
  {4 literal-packet-value

   0 (aggregate-sub-packets numbers/sum)
   1 (aggregate-sub-packets numbers/product)
   2 (aggregate-sub-packets #(apply min %))
   3 (aggregate-sub-packets #(apply max %))

   5 (pred-sub-packet-value-fn >)
   6 (pred-sub-packet-value-fn <)
   7 (pred-sub-packet-value-fn =)})

(defn packet-value
  [{:packet/keys [type] :as packet}]
  ((packet-type->value-fn type) packet))

(defn part2-solution
  []
  (utils/with-input
    (fn [lines]
      (->> lines
           parse-input
           decode-packet
           first
           packet-value))))

(comment
  (time (part1-solution))
  (time (part2-solution))

  *e)
