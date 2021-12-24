(ns advent-of-code.data-structures.heap-test
  (:require [advent-of-code.data-structures.heap :as heap]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]))

(def min-heap
  "    1
      / \\
     6   3
    /   / \\
   8   5   9"
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