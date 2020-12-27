(ns advent-of-code.2020.day17
  (:require [advent-of-code.utils :as utils]
            [taoensso.tufte :as tufte :refer [defnp fnp p profiled profile]]
            [clojure.core.match :as match]))

(defn n-dimensional-map
  [dimensions-count]
  {:dimensions dimensions-count})

(defn set-val
  [m indices val]
  (assert
   (= (count indices) (:dimensions m))
   (format "Received indices for a %d-dimensional map, but input is a %d-dimensional map"
           (count indices) (:dimensions m)))
  (letfn [(set-val* [m indices val]
            (let [indices-dimensions (count indices)
                  [idx & rest-indices] indices]
              (if (= 1 indices-dimensions)
                (assoc m idx val)
                (update m idx (fnil set-val (n-dimensional-map (dec indices-dimensions))) rest-indices val))))]
    (set-val* m indices val)))

(defn remove-val
  [m indices]
  (assert
   (= (count indices) (:dimensions m))
   (format "Received indices for a %d-dimensional map, but input is a %d-dimensional map"
           (count indices) (:dimensions m)))
  (letfn [(remove-val* [m indices]
            (let [indices-dimensions (count indices)
                  [idx & rest-indices] indices]
              (cond
                (= 1 indices-dimensions)
                (dissoc m idx)

                (contains? m idx)
                (update m idx remove-val rest-indices)

                :else
                m)))]
    (remove-val* m indices)))

(defn flatten-n-dimensional-map*
  [{:keys [dimensions] :as m}]
  (if (= dimensions 1)
    (->> (dissoc m :dimensions)
         (map (juxt (comp list first) second)))
    (->> (dissoc m :dimensions)
         (map (fn [[idx inner-m]]
                (map (fn [[inner-idx val]]
                       [(cons idx inner-idx) val])
                     (flatten-n-dimensional-map* inner-m))))
         utils/lazy-cat*)))

(defn flatten-n-dimensional-map
  [m]
  (into {} (flatten-n-dimensional-map* m)))

(def active-state \#)

(defn parse-row
  [{:keys [dimensions] :as m} x-idx row]
  (->> row
       (map-indexed vector)
       (reduce (fn [m [y-idx val]]
                 (if (= active-state val)
                   (set-val m (into [x-idx y-idx] (repeat (- dimensions 2) 0)) :active)
                   m))
               m)))

(defn parse-map
  [dimensions lines]
  (->> lines
       (map-indexed vector)
       (reduce (fn [m [x-idx row]]
                 (parse-row m x-idx row))
               (n-dimensional-map dimensions))))

(def variations-for-dimensions
  ^{:arglists '([dimensions])}
  (letfn [(variations-for-dimensions* [dimensions]
            (remove (partial apply = 0)
                    (reduce (fn [res deltas]
                              (->> deltas
                                   (map (fn [partial-variation]
                                          (map #(conj % partial-variation) res)))
                                   utils/lazy-cat*))
                            [[-1] [0] [1]]
                            (repeat (dec dimensions) [-1 0 1]))))]
    (memoize variations-for-dimensions*)))

(defn adjacent-spaces
  [indices]
  (let [possible-variations (variations-for-dimensions (count indices))]
    (map (fn [variation]
           (mapv + indices variation))
          possible-variations)))

(defn states-to-be-computed
  [m]
  (let [active-spaces-in-map (map first (flatten-n-dimensional-map* m))
        all-adjacent-spaces (->> active-spaces-in-map
                                 (map adjacent-spaces)
                                 utils/lazy-cat*)]
    (into #{} (lazy-cat active-spaces-in-map all-adjacent-spaces))))

(defn next-space-state
  [m indices]
  (let [adjacent-active-spaces (->> (adjacent-spaces indices)
                                    (filterv #(some? (get-in m %))))
        current-state (get-in m indices :inactive)]
    (match/match [current-state (count adjacent-active-spaces)]
      [:active (:or 2 3)] :active
      [:inactive 3] :active
      :else :inactive)))

(defn next-map-state
  [m]
  (->> m
       states-to-be-computed
       (map (juxt identity (partial next-space-state m)))
       (reduce (fn [m [indices next-state]]
                 (let [old-state (get-in m indices :inactive)]
                   (match/match [old-state next-state]
                     [:active :inactive] (remove-val m indices)
                     [:inactive :active] (set-val m indices :active)
                     :else m)))
               m)))

(defn state-after-cycles
  [initial-map n]
  (reduce (fn [m _] (next-map-state m))
          initial-map
          (repeat n nil)))

(defnp part1-solution
  [lines]
  (let [m (parse-map 3 lines)]
    (->> (state-after-cycles m 6)
         flatten-n-dimensional-map*
         count)))

(defnp part2-solution
  [lines]
  (let [m (parse-map 4 lines)]
    (->> (state-after-cycles m 6)
         flatten-n-dimensional-map*
         count)))

(defn day-solution
  []
  (utils/with-lines "2020/day17.txt"
    (fn [lines]
      #tap (part1-solution lines)
      #tap (part2-solution lines)))
  nil)

(comment
  (tufte/add-basic-println-handler! {})
  (profile {} (day-solution)))