(ns advent-of-code-2020.utils
  (:require [clojure.java.io :as java.io]))

(defmacro with-lines
  [path & body]
  `(with-open [rdr# (java.io/reader ~path)]
     (let [~'lines (line-seq rdr#)]
       ~@body)))

(comment
  (macroexpand-1
   '(with-lines "resources/day1.txt"
      (->> lines
           (map identity)))))