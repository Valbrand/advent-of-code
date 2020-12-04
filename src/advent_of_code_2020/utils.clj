(ns advent-of-code-2020.utils
  (:require [clojure.java.io :as java.io]
            [clojure.pprint :as pp]))

(defn with-lines
  [path f]
  (with-open [rdr (java.io/reader path)]
    (let [lines (line-seq rdr)]
       (f lines))))

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
  (macroexpand '(tap (-> {:a 1} keys set))))