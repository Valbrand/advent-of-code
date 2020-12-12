(ns advent-of-code.numbers)

(defn pair-summing
  [goal ints]
  (loop [[n & rest] ints
         numbers-we-need #{}]
    (cond
      (nil? n)
      nil

      (contains? numbers-we-need n)
      [n (- goal n)]

      :else
      (recur rest (conj numbers-we-need (- goal n))))))

(defn sum
  [numbers]
  (reduce + 0 numbers))

(defn product
  [numbers]
  (reduce * 1 numbers))

(defn pow
  [base power]
  (if (instance? clojure.lang.BigInt base)
    (bigint (.pow (biginteger base) power))
    (Math/pow base power)))

(defn abs
  [n]
  (if (neg? n) (- n) n))

(comment
  (class 2N)
  (pow 2N 3))