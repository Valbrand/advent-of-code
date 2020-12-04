(ns advent-of-code-2020.utils
  (:require [clojure.java.io :as java.io]
            [clojure.pprint :as pp]))

(defmacro with-lines
  [path & body]
  `(with-open [rdr# (java.io/reader ~path)]
     (let [~'lines (line-seq rdr#)]
       ~@body)))

(defmacro tap
  [x]
  (let [x-raw (with-out-str (pp/pprint x))]
    `(do
       (print ~x-raw)
       (pp/pprint ~x)
       ~x)))

(defn parse-int
  [str]
  (Integer/parseInt str))

(comment
  (-> {:a 1} keys set tap identity tap)
  (macroexpand '(tap (-> {:a 1} keys set)))
  (macroexpand-1
   '(with-lines "resources/day1.txt"
      (->> lines
           (map identity)))))