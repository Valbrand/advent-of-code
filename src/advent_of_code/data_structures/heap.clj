(ns advent-of-code.data-structures.heap)

(defn parent
  [heap index]
  (when (pos? index)
    (quot (dec index) 2)))

(defn left-child
  [heap index]
  (let [child-index (inc (* index 2))]
    (when (< child-index (count heap))
      child-index)))

(defn right-child
  [heap index]
  (let [child-index (+ 2 (* index 2))]
    (when (< child-index (count heap))
      child-index)))

(defn- swap-elements
  [elements index-a index-b]
  (-> elements
      (assoc index-a (get elements index-b))
      (assoc index-b (get elements index-a))))

(defn- sift-up
  [elements compare-fn index]
  (let [element (get elements index)
        parent-index (parent elements index)]
    (if (and (some? parent-index)
                    (neg? (compare-fn element (get elements parent-index))))
      (recur (swap-elements elements index parent-index)
             compare-fn
             parent-index)
      elements)))

(defn sift-down
  [elements compare-fn index]
  (let [element (get elements index)
        left-child-idx (left-child elements index)
        right-child-idx (right-child elements index)]
    (cond
      (and (nil? left-child-idx)
           (nil? right-child-idx))
      elements

      (nil? right-child-idx)
      (if (pos? (compare-fn element (get elements left-child-idx)))
        (recur (swap-elements elements index left-child-idx)
               compare-fn
               left-child-idx)
        elements)

      (nil? left-child-idx)
      (if (pos? (compare-fn element (get elements right-child-idx)))
        (recur (swap-elements elements index right-child-idx)
               compare-fn
               right-child-idx)
        elements)

      :else
      (let [left-child (get elements left-child-idx)
            right-child (get elements right-child-idx)
            min-child-idx (if (neg? (compare-fn left-child right-child))
                            left-child-idx
                            right-child-idx)]
        (if (pos? (compare-fn element (get elements min-child-idx)))
          (recur (swap-elements elements index min-child-idx)
                 compare-fn
                 min-child-idx)
          elements)))))

(defn- build-min-heap
  ([elements compare-fn]
   (build-min-heap elements compare-fn (quot (count elements) 2)))
  ([elements compare-fn idx]
   (let [element (get elements idx)
         left-child-idx (left-child elements idx)
         right-child-idx (right-child elements idx)]
     (cond
       (neg? idx)
       elements

       (and (nil? left-child-idx)
            (nil? right-child-idx))
       (recur elements compare-fn (dec idx))

       (nil? right-child-idx)
       (if (pos? (compare-fn element (get elements left-child-idx)))
         (recur (swap-elements elements idx left-child-idx) compare-fn (dec idx))
         (recur elements compare-fn (dec idx)))

       (nil? left-child-idx)
       (if (pos? (compare-fn element (get elements right-child-idx)))
         (recur (swap-elements elements idx right-child-idx) compare-fn (dec idx))
         (recur elements compare-fn (dec idx)))

       :else
       (let [left-child (get elements left-child-idx)
             right-child (get elements right-child-idx)
             min-child-idx (if (neg? (compare-fn left-child right-child))
                             left-child-idx
                             right-child-idx)]
         (if (pos? (compare-fn element (get elements min-child-idx)))
           (recur (swap-elements elements idx min-child-idx) compare-fn (dec idx))
           (recur elements compare-fn (dec idx))))))))

(defprotocol ^:private EditableHeap
  (with-compare-fn [_ compare-fn])
  (update-elements [_ update-fn]))

(deftype VectorHeap
  [elements compare-fn]

  Object
  (toString [this]
    (str "VectorHeap<" (seq this) ">"))

  EditableHeap
  (with-compare-fn [_ new-compare-fn]
    (VectorHeap. (build-min-heap elements new-compare-fn)
                 new-compare-fn))
  (update-elements [_ update-fn]
    (VectorHeap. (build-min-heap (update-fn elements) compare-fn)
                 compare-fn))

  clojure.lang.IPersistentCollection
  (count [_]
    (count elements))
  (cons [_ element]
    (VectorHeap. (-> elements
                     (conj element)
                     (sift-up compare-fn
                              (count elements)))
                 compare-fn))
  (empty [_]
    (VectorHeap. (empty elements)
                 compare-fn))
  (equiv [this other]
    (and (seqable? other)
         (= (seq this)
            (seq other))))
  (seq [_]
    (seq elements))

  clojure.lang.IPersistentStack
  (peek [_this]
    (first elements))
  (pop [_this]
    (VectorHeap. (-> elements
                     (swap-elements 0 (dec (count elements)))
                     (subvec 0 (dec (count elements)))
                     (sift-down compare-fn 0))
                 compare-fn)))

(defn min-heap-by
  [compare-fn & elements]
  (VectorHeap. (build-min-heap (vec elements) compare-fn)
               compare-fn))

(defn min-heap
  [& elements]
  (into (VectorHeap. [] compare)
        elements))

(defn- heap-map-compare
  ([]
   (heap-map-compare compare))
  ([compare-fn]
   (heap-map-compare {} compare-fn))
  ([inner-map compare-fn]
   (fn [k1 k2]
     (compare-fn (get inner-map k1)
                 (get inner-map k2)))))

(defn- index-of
  [elements element]
  (some (fn [index]
          (when (= element (get elements index))
            index))
        (range (count elements))))

(deftype HeapMap
  [inner-map v-heap compare-fn]

  Object
  (toString [_]
    (str "HeapMap:\n"
         "inner map:" (str inner-map) "\n"
         "v-heap:" (str (seq v-heap)) "\n"
         "==="))

  clojure.lang.IPersistentMap
  (assocEx [this k v]
    (let [_ (.assocEx inner-map k v)]
      (assoc this k v)))
  (without [_ k]
    (let [new-inner-map (dissoc inner-map k)]
      (HeapMap. new-inner-map
                (if (contains? inner-map k)
                  (let [elements (.elements v-heap)
                        idx-to-remove (index-of (.elements v-heap) k)
                        new-count (dec (count elements))]
                    (VectorHeap. (-> (swap-elements elements idx-to-remove new-count)
                                     (subvec 0 new-count)
                                     (sift-down compare-fn idx-to-remove))
                                 (heap-map-compare new-inner-map
                                                   compare-fn)))
                  v-heap)
                compare-fn)))
  (containsKey [_ key]
    (contains? inner-map key))
  (entryAt [_ key]
    (.entryAt inner-map key))
  (assoc [_ k v]
    (let [new-inner-map (assoc inner-map k v)
          new-compare-fn (heap-map-compare new-inner-map
                                           compare-fn)
          elements (.elements v-heap)]
      (HeapMap. new-inner-map
                (VectorHeap. (-> elements
                                 (conj k)
                                 (sift-up new-compare-fn (count elements)))
                             new-compare-fn)
                compare-fn)))
  (valAt [_ key]
    (get inner-map key))
  (valAt [_ key not-found]
    (get inner-map key not-found))
  (count [_]
    (count inner-map))
  (cons [this [k v]]
    (assoc this k v))
  (empty [_]
    (HeapMap. (empty inner-map)
              (empty v-heap)
              compare-fn))
  (equiv [this other]
    (and (seqable? other)
         (= (seq this)
            (seq other))))
  (seq [_]
    (map (fn [k]
           (.entryAt inner-map k))
         (seq v-heap)))

  clojure.lang.IPersistentStack
  (peek [_]
    (.entryAt inner-map (peek v-heap)))
  (pop [_]
    (let [k (peek v-heap)
          new-inner-map (dissoc inner-map k)
          new-v-heap (pop v-heap)]
      (HeapMap. new-inner-map
                (VectorHeap. (.elements new-v-heap)
                             (heap-map-compare new-inner-map compare-fn))
                compare-fn))))

(defn heap-map
  [& keyvals]
  {:pre (even? (count keyvals))}
  (reduce (fn [m [k v]]
            (assoc m k v))
          (HeapMap. {}
                    (min-heap-by (heap-map-compare))
                    compare)
          (partition 2 keyvals)))

(defn heap-map-by
  [compare-fn & keyvals]
  {:pre (even? (count keyvals))}
  (reduce (fn [m [k v]]
            (assoc m k v))
          (HeapMap. {}
                    (min-heap-by (heap-map-compare compare-fn))
                    compare-fn)
          (partition 2 keyvals)))
