(ns advent-of-code.utils
  (:require [clojure.java.io :as java.io]
            [clojure.pprint :as pp]))

(defn with-lines
  [path f]
  (with-open [rdr (java.io/reader (str "resources/" path))]
    (let [lines (line-seq rdr)]
      (f lines))))

(defmacro tap
  [x]
  (let [x-raw (with-out-str (pp/pprint x))]
    `(let [x-val# ~x]
       (print ~x-raw)
       (pp/pprint x-val#)
       x-val#)))

(defn parse-int
  [str]
  (Integer/parseInt str))

(defn split-by-empty-lines
  [lines]
  (->> lines
       (partition-by empty?)
       (filter #(some seq %))))

(comment
  (do
    (defn plus! [& args]
      (println "plus!")
      (apply + args))
    (tap (+ 1 (tap (plus! 2 3)))))

  (= [["a" "z"] ["b"]]
     (split-by-empty-lines ["a" "z" "" "" "b"]))

  (-> {:a 1} keys set tap identity tap)
  (macroexpand '(tap (-> {:a 1} keys set))))