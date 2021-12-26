(ns advent-of-code.2021.day17-test
  (:require [advent-of-code.2021.day17 :as day17]
            [clojure.test :refer [deftest are is testing]]
            [matcher-combinators.test :refer [match?]]))

(deftest inside-target?-test
  (are [target pos expected]
    (match? expected
            (day17/inside-target? target pos))
    [2 4] 1 false
    [2 4] 2 true
    [2 4] 3 true
    [2 4] 4 true
    [2 4] 5 false
    [-4 -2] -1 false
    [-4 -2] -2 true
    [-4 -2] -3 true
    [-4 -2] -4 true
    [-4 -2] -5 false))

(deftest farthest-reachable-point
  (is (match? 0 (day17/farthest-reachable-point 0)))
  (is (match? 6 (day17/farthest-reachable-point 3)))
  (is (match? -6 (day17/farthest-reachable-point -3))))

(deftest smallest-possible-x-speed-test
  (are [target speed]
    (match? speed (day17/smallest-possible-x-speed target))
    ;; when target starts with negative coordinates, 
    ;; the smallest possible speed is the speed 
    ;; that hits the edge of the window in the first step
    [-10 -5] -10
    [-10 15] -10
    ;; when target starts with positive coordinates, 
    ;; the smallest possible speed is the increment 
    ;; of the largest speed that cannot reach 
    ;; the start of the target
    [11 15] 5
    [7 15] 4))

(deftest largest-possible-x-speed-test
  (are [target speed]
    (match? speed (day17/largest-possible-x-speed target))
    ;; when target ends with positive coordinates, 
    ;; the largest possible speed is the speed 
    ;; that hits the edge of the target in the first step
    [5 10] 10
    [-5 10] 10
    ;; when target ends with negative coordinates, 
    ;; the largest possible speed is the decrement
    ;; of the first speed that cannot reach the end
    ;; of the target
    [-10 -7] -4
    [-20 -11] -5))

(deftest smallest-possible-y-speed-test
  (are [target speed]
    (match? speed (day17/smallest-possible-y-speed target))
    ;; when target starts with negative coordinates, 
    ;; the smallest possible speed is the speed 
    ;; that hits the edge of the window in the first step
    [-10 -5] -10
    [-10 15] -10
    ;; when target starts with positive coordinates, 
    ;; the smallest possible speed is the increment 
    ;; of the largest speed that cannot reach 
    ;; the start of the target
    [11 15] 5
    [7 15] 4))

(deftest largest-possible-y-speed-test
  (are [target speed]
       (match? speed (day17/largest-possible-y-speed target))
    ;; when target ends with positive coordinates, 
    ;; the largest possible speed is the greater
    ;; of the absolute values of the target limits.
    ;; If the start of the window is negative, the
    ;; result is decremented.
    [5 10] 10
    [-5 10] 10
    [-15 10] 14
    ;; when target ends with negative coordinates, 
    ;; the largest possible speed is the decrement
    ;; of the absolute value of the start of
    ;; the target window
    [-10 -7] 9
    [-20 -11] 19))

(comment
  (clojure.test/run-tests))