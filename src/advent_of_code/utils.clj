(ns advent-of-code.utils
  (:require [advent-of-code.numbers :as numbers]
            [clojure.java.io :as java.io]
            [clojure.pprint :as pp]
            [clojure.string :as string]))

(def digits-pattern #"\d+")

(defn parse-integers-line
  ([line]
   (parse-integers-line line #" "))
  ([line separator]
   (->> (string/split line separator)
        (remove empty?)
        (map numbers/parse-int))))

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

(defn find-first
  [pred coll]
  (first (drop-while (complement pred) coll)))

(defn zip
  [coll-a coll-b]
  (map vector coll-a coll-b))

(defn median
  "Guaranteed to work only for odd-sized collections"
  ([coll]
   (median coll false))
  ([coll sorted?]
   (let [coll (if sorted?
                coll
                (sort coll))
         median-index (/ (count coll) 2)]
     (nth coll median-index))))

(defn queue
  ([]
   clojure.lang.PersistentQueue/EMPTY)
  ([items]
   (into (queue) items)))

(defn cartesian-product
  [xs ys]
  (for [x xs, y ys]
    [x y]))

(defn surrounding-matrix-indices
  [[base-x base-y] matrix]
  (for [x (range (dec base-x) (+ 2 base-x))
        y (range (dec base-y) (+ 2 base-y))
        :when (and (>= x 0)
                   (>= y 0)
                   (or (not= x base-x)
                       (not= y base-y))
                   (< x (count matrix))
                   (< y (count (get matrix x))))]
    [x y]))

(defmacro with-input
  [f]
  (let [[day year] (-> (str *ns*)
                       (string/split #"\.")
                       reverse)
        path (str year "/" day ".txt")]
    `(with-lines ~path
       ~f)))

(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-until pred (rest s)))))))

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