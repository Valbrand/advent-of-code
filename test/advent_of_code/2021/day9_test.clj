(ns advent-of-code.2021.day9-test
  (:require [advent-of-code.2021.day9 :as day9]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]))

(def test-depth-map
  [[2 1 9 9 9 4 3 2 1 0]
   [3 9 8 7 8 9 4 9 2 1]
   [9 8 5 6 7 8 9 8 9 2]
   [8 7 6 7 8 9 6 7 8 9]
   [9 8 9 9 9 6 5 6 7 8]])

(deftest neighbors-test
  (testing "top left corner should only return right and down neighbors"
    (is (match? [[0 1] [1 0]]
                (day9/neighbors test-depth-map [0 0]))))
  
  (testing "bottom right corner should only return up and left neighbors"
    (is (match? [[3 9] [4 8]]
                (day9/neighbors test-depth-map [4 9]))))
  
  (testing "a point that's not near the borders of the map returns all four neighbors"
    (is (match? [[0 1] [1 2] [2 1] [1 0]]
                (day9/neighbors test-depth-map [1 1])))))

(deftest find-low-points-test
  (is (match? [[0 1] [0 9] [2 2] [4 6]]
              (day9/find-low-points test-depth-map))))

(deftest lazy-coordinates-seq
  (is (match? [[0 0] [0 1] [1 0] [1 1]]
              (day9/lazy-coordinates-seq [[1 2] [3 4]]))))

(deftest basin-for-low-point-test
  (is (match? #{[0 0] [0 1] [1 0]}
              (day9/basin-for-low-point test-depth-map [0 1])))
  
  (is (match? #{[0 5] [0 6] [0 7] [0 8] [0 9]
                [1 6] [1 8] [1 9]
                [2 9]}
              (day9/basin-for-low-point test-depth-map [0 9])))
  
  (is (match? #{[1 2] [1 3] [1 4]
                [2 1] [2 2] [2 3] [2 4] [2 5]
                [3 0] [3 1] [3 2] [3 3] [3 4]
                [4 1]}
              (day9/basin-for-low-point test-depth-map [2 2])))
  
  (is (match? #{[2 7]
                [3 6] [3 7] [3 8]
                [4 5] [4 6] [4 7] [4 8] [4 9]}
              (day9/basin-for-low-point test-depth-map [4 6]))))

(comment
  (clojure.test/run-tests))