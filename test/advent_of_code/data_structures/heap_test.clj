(ns advent-of-code.data-structures.heap-test
  (:require [advent-of-code.data-structures.heap :as heap]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]))

(def min-heap
  (heap/min-heap 9 8 5 1 6 3))

(def heap-map
  (heap/heap-map :a 9 :b 8 :c 5 :d 1 :e 6 :f 3))

(deftest min-heap-peek-test
  (is (match? 1
              (peek min-heap))))

(deftest min-heap-pop-test
  (testing "pop removes an element from the heap"
    (is (match? (dec (count min-heap))
                (count (pop min-heap)))))
  
  (testing "the new root of the heap is the second lesser element"
    (is (match? 3
                (peek (pop min-heap))))))

(deftest heap-map-peek-test
  (is (match? [:d 1]
              (peek heap-map))))

(deftest heap-map-pop-test
  (testing "pop removes an element from the heap"
    (is (match? (dec (count heap-map))
                (count (pop heap-map)))))

  (testing "the new root of the heap is the second lesser element"
    (is (match? [:f 3]
                (peek (pop heap-map))))))

(deftest heap-map-assoc-test
  (testing "Adding a new key, bigger than the current top of the heap"
    (let [heap-map-after-assoc (assoc heap-map :new-key 2)]
      (is (match? true
                  (contains? heap-map-after-assoc :new-key)))

      (is (match? 2
                  (get heap-map-after-assoc :new-key)))

      (is (match? [:d 1]
                  (peek heap-map-after-assoc)))))
  
  (testing "Adding a new key, smaller than the current top of the heap"
    (let [heap-map-after-assoc (assoc heap-map :new-key 0)]
      (is (match? true
                  (contains? heap-map-after-assoc :new-key)))

      (is (match? 0
                  (get heap-map-after-assoc :new-key)))

      (is (match? [:new-key 0]
                  (peek heap-map-after-assoc)))))
  
  (testing "assoc'ing an existing key (not associated to top of heap), bigger than the current top of the heap"
    (let [heap-map-after-assoc (assoc heap-map :f 10)]
      (is (match? (count heap-map)
                  (count heap-map-after-assoc)))

      (is (match? 10
                  (get heap-map-after-assoc :f)))

      (is (match? [:d 1]
                  (peek heap-map-after-assoc)))))
  
  (testing "assoc'ing an existing key (not associated to top of heap), smaller than the current top of the heap"
    (let [heap-map-after-assoc (assoc heap-map :f 0)]
      (is (match? (count heap-map)
                  (count heap-map-after-assoc)))

      (is (match? 0
                  (get heap-map-after-assoc :f)))

      (is (match? [:f 0]
                  (peek heap-map-after-assoc)))))
  
  (testing "assoc'ing an existing key (associated to top of heap), bigger than the current top of the heap"
    (let [heap-map-after-assoc (assoc heap-map :d 10)]
      (is (match? (count heap-map)
                  (count heap-map-after-assoc)))

      (is (match? 10
                  (get heap-map-after-assoc :d)))

      (is (match? [:f 3]
                  (peek heap-map-after-assoc)))))
  
  (testing "assoc'ing an existing key (associated to top of heap), smaller than the current top of the heap"
    (let [heap-map-after-assoc (assoc heap-map :d 0)]
      (is (match? (count heap-map)
                  (count heap-map-after-assoc)))

      (is (match? 0
                  (get heap-map-after-assoc :d)))

      (is (match? [:d 0]
                  (peek heap-map-after-assoc))))))

(deftest heap-map-dissoc-test
  (testing "Removing a key not in the heap-map returns the same instance"
    (is (identical? heap-map
                    (dissoc heap-map :not-in-heap-map))))
  
  (testing "The top of the heap is unchanged if the removed key is not the top of the heap itself"
    (let [heap-map-after-dissoc (dissoc heap-map :b)]
      (is (match? false
                  (contains? heap-map-after-dissoc :b)))

      (is (match? [:d 1]
                  #_{:clj-kondo/ignore [:type-mismatch]}
                  (peek heap-map-after-dissoc)))))

  (testing "After removing the top of the heap, it changes"
    (let [heap-map-after-dissoc (dissoc heap-map :d)]
      (is (match? false
                  (contains? heap-map-after-dissoc :d)))

      (is (match? [:f 3]
                  #_{:clj-kondo/ignore [:type-mismatch]}
                  (peek heap-map-after-dissoc))))))

(deftest parent-test
  (is (match? nil
              (heap/parent min-heap 0)))
  
  (is (match? 0
              (heap/parent min-heap 1)))
  
  (is (match? 0
              (heap/parent min-heap 2)))
  
  (is (match? 1
              (heap/parent min-heap 3)))
  
  (is (match? 1
              (heap/parent min-heap 4)))
  
  (is (match? 2
              (heap/parent min-heap 5))))

(deftest left-child
  (is (match? 1
              (heap/left-child min-heap 0)))

  (is (match? 3
              (heap/left-child min-heap 1)))

  (is (match? 5
              (heap/left-child min-heap 2)))

  (is (match? nil
              (heap/left-child min-heap 3)))

  (is (match? nil
              (heap/left-child min-heap 4)))

  (is (match? nil
              (heap/left-child min-heap 5))))

(deftest right-child
  (is (match? 2
              (heap/right-child min-heap 0)))

  (is (match? 4
              (heap/right-child min-heap 1)))

  (is (match? nil
              (heap/right-child min-heap 2)))

  (is (match? nil
              (heap/right-child min-heap 3)))

  (is (match? nil
              (heap/right-child min-heap 4)))

  (is (match? nil
              (heap/right-child min-heap 5))))

(comment
  (clojure.test/run-tests))