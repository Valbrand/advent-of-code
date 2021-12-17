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

(defn divisible?
  [n by]
  (zero? (mod n by)))

(defn descending-numeric-compare
  [a b]
  (- b a))

(defn extended-gcd
  [a b]
  (loop [[old-r old-s old-t] [a 1N 0N]
         [r s t] [b 0N 1N]]
    (if (zero? r)
      [old-r old-s old-t]
      (let [q (quot old-r r)
            next-val (fn [old-val current-val]
                       (- old-val (* q current-val)))]
        (recur [r s t]
               [(next-val old-r r)
                (next-val old-s s)
                (next-val old-t t)])))))

(defn bitwise-and
  [num & nums]
  (bigint
   (reduce (fn [result n]
             (.and (biginteger n) result))
           (biginteger num)
           nums)))

(defn bitwise-or
  [num & nums]
  (bigint
   (reduce (fn [result n]
             (.or (biginteger n) result))
           (biginteger num)
           nums)))

(defn parse-int
  ([s]
   (bigint s))
  ([s radix]
   (case radix
     2 (read-string (str "2r" s))
     (Integer/parseInt s radix))))

(defn char->int
  [char]
  {:pre (<= (int \0) (int char) (int \9))}
  (- (int char)
     (int \0)))

(comment
  (extended-gcd 18N 0N)
  (class 2N)
  (pow 2N 3))