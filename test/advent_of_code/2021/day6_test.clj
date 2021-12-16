(ns advent-of-code.2021.day6-test
  (:require [advent-of-code.2021.day6 :as day6]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]))

(deftest parse-input
  (is (match? [1 2 2 1 1 1 1 0 0]
              (day6/parse-input ["0,1,2,3,4,5,6,1,2"]))))

(deftest state-after-n-days
  (testing "without any lanternfish generation"
    (is (match? [1 1 1 1 1 0 0 0 0]
                (day6/state-after-n-days 1 [0 1 1 1 1 1 0 0 0]))))
  
  (testing "with lanternfish generation (< 7 days)"
    (is (match? [1 1 1 1 0 0 1 0 1]
                (day6/state-after-n-days 2 [0 1 1 1 1 1 0 0 0]))))
  
  (testing "with lanternfish generation (>= 7 days)"
    (is (match? [0 1 1 2 2 2 1 1 0]
                (day6/state-after-n-days 7 [0 1 1 1 1 1 0 0 0])))))

(comment
  (clojure.test/run-tests))