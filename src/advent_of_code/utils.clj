(ns advent-of-code.utils
  (:require [clojure.java.io :as java.io]
            [clojure.pprint :as pp]))

(def digits-pattern #"\d+")

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

(defn tap-reader
  [x]
  `(let [x-val# ~x]
     (pp/pprint '~x)
     (pp/pprint x-val#)
     x-val#))

(defn parse-int
  ([s]
   (bigint s))
  ([s radix]
   (case radix
     2 (read-string (str "2r" s))
     (Integer/parseInt s radix))))

(defn split-by-empty-lines
  [lines]
  (->> lines
       (partition-by empty?)
       (filter #(some seq %))))

(defn map-vals*
  "Applies f to the values of a map, returns a lazy seq of the map entries"
  [f m]
  (map (juxt first (comp f second)) m))

(defn map-vals
  "Applies f to the values of a map, returns a map"
  [f m]
  (into {} (map-vals* f m)))

(defn filter-vals*
  "Filters map entries for which pred returns a truthy value. Returns a lazy seq of the entries"
  [pred m]
  (filter (comp pred second) m))

(defn filter-vals
  "Filters map entries for which pred returns a truthy value. Returns a map"
  [pred m]
  (into {} (filter-vals* pred m)))

(defn index-of
  [item coll]
  (->> coll
       (map-indexed vector)
       (filter (comp #{item} second))
       ffirst))

(defn lazy-cat*
  [colls]
  (when-let [[coll & rest] (seq colls)]
    (lazy-cat coll (lazy-cat* rest))))

(defn singleton?
  [coll]
  (= 1 (count coll)))

(defn unchunk [s]
  (when (seq s)
    (lazy-seq
     (cons (first s)
           (unchunk (next s))))))

(comment
  (index-of 1 [2 3 1])
  (map-vals inc {:a 1, :b 0})
  (do
    (defn plus! [& args]
      (println "plus!")
      (apply + args))
    (tap (+ 1 (tap (plus! 2 3)))))

  (= [["a" "z"] ["b"]]
     (split-by-empty-lines ["a" "z" "" "" "b"]))

  (-> {:a 1} keys set tap identity tap)
  (macroexpand '(tap (-> {:a 1} keys set))))