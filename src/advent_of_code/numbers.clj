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
