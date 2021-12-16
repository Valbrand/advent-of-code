(ns advent-of-code.2021.day7-test
  (:require [advent-of-code.2021.day7 :as day7]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]))

(deftest compute-distances-test
  (testing "single position should yield a map with distance 0"
    (is (match? {10 0}
                (day7/compute-required-fuel [10]))))
  
  (testing "with two positions, map should show same distance for both positions"
    (is (match? {5 5
                 10 5}
                (day7/compute-required-fuel [5 10]))))
  
  (testing "with more than one different position, repeated positions should be computed repeatedly to other positions"
    (is (match? {5 5
                 10 10}
                (day7/compute-required-fuel [5 10 5])))))

(deftest compute-distances-test-with-fuel-fn
  (letfn [(fuel-fn [_] 1)]
    (testing "single position should yield a map with distance 0"
      (is (match? {10 0}
                  (day7/compute-required-fuel fuel-fn [10]))))

    (testing "with two positions, map should show same distance for both positions"
      (is (match? {5 1
                   10 1}
                  (day7/compute-required-fuel fuel-fn [5 10]))))

    (testing "with more than one different position, repeated positions should be computed repeatedly to other positions"
      (is (match? {5 1
                   10 2}
                  (day7/compute-required-fuel fuel-fn [5 10 5]))))))

(comment
  (clojure.test/run-tests))