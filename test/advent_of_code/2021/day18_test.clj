(ns advent-of-code.2021.day18-test
  (:require [advent-of-code.2021.day18 :as day18]
            [clojure.test :refer [deftest are is testing]]
            [clojure.zip :as zip]
            [matcher-combinators.test :refer [match?]]))

(deftest simple-add-test
  (is (match? [[0 1] [2 3]]
              (day18/add [0 1] [2 3])))
  
  (is (match? [[[[1 1] [2 2]] [3 3]] [4 4]]
              (day18/add [1 1] [2 2] [3 3] [4 4]))))

(deftest complex-add-test
  (is (match? [[[[3 0] [5 3]] [4 4]] [5 5]]
              (day18/add [1 1] [2 2] [3 3] [4 4] [5 5])))

  (is (match? [[[[5 0] [7 4]] [5 5]] [6 6]]
              (day18/add [1 1] [2 2] [3 3] [4 4] [5 5] [6 6])))
  
  (is (match? [[[[8 7] [7 7]] [[8 6] [7 7]]] [[[0 7] [6 6]] [8 7]]]
              (day18/add [[[0 [4 5]] [0 0]] [[[4 5] [2 6]] [9 5]]]
                         [7 [[[3 7] [4 3]] [[6 3] [8 8]]]]
                         [[2 [[0 8] [3 4]]] [[[6 7] 1] [7 [1 6]]]]
                         [[[[2 4] 7] [6 [0 5]]] [[[6 8] [2 8]] [[2 1] [4 5]]]]
                         [7 [5 [[3 8] [1 4]]]]
                         [[2 [2 2]] [8 [8 1]]]
                         [2 9]
                         [1 [[[9 3] 9] [[9 0] [0 7]]]]
                         [[[5 [7 4]] 7] 1]
                         [[[[4 2] 2] 6] [8 7]]))))

(def snailfish-number
  [[0 1] [[2 3] 4]])
(def snailfish-number-zip
  (zip/vector-zip snailfish-number))
(def zipper-at-node
  {0 (-> snailfish-number-zip
         zip/down zip/down)
   1 (-> snailfish-number-zip
         zip/down zip/down zip/right)
   2 (-> snailfish-number-zip
         zip/down zip/right zip/down zip/down)
   3 (-> snailfish-number-zip
         zip/down zip/right zip/down zip/down zip/right)
   4 (-> snailfish-number-zip
         zip/down zip/right zip/down zip/right)
   [0 1] (-> snailfish-number-zip
             zip/down)
   [2 3] (-> snailfish-number-zip
             zip/down zip/right zip/down)})

(deftest number-to-the-left
  (are [from result]
    (match? result
            (some-> from
                    zipper-at-node
                    day18/number-to-the-left
                    zip/node))
    0 nil
    1 0
    2 1
    3 2
    4 3
    [0 1] nil
    [2 3] 1))

(deftest number-to-the-right
  (are [from result]
    (match? result
            (some-> from
                    zipper-at-node
                    day18/number-to-the-right
                    zip/node))
    0 1
    1 2
    2 3
    3 4
    4 nil
    [0 1] 2
    [2 3] 4))

(deftest depth
  (are [node result]
    (match? result
            (some-> node
                    zipper-at-node
                    day18/depth))
    [0 1] 1
    [2 3] 2))

(deftest simple-pair?
  (are [pair result]
    (match? result
            (day18/simple-pair? pair))
    0 false
    [0 1] true
    [[2 3] 4] false
    [2 [3 4]] false
    [[2 3] [4 5]] false))

(deftest loc-to-explode
  (is (match? nil
              (day18/loc-to-explode snailfish-number-zip)))

  (is (match? [9 8]
              (-> [[[[[9 8] 1] 2] 3] 4]
                  zip/vector-zip
                  day18/loc-to-explode
                  zip/node))))

(deftest loc-to-split
  (is (match? nil
              (day18/loc-to-split snailfish-number-zip)))

  (is (match? 15
              (-> [[[[0 7] 4] [15 [0 13]]] [1 1]]
                  zip/vector-zip
                  day18/loc-to-split
                  zip/node))))

(deftest magnitude
  (are [number magnitude]
    (match? magnitude
            (day18/magnitude number))
    [[1 2] [[3 4] 5]] 143
    [[[[0 7] 4] [[7 8] [6 0]]] [8 1]] 1384
    [[[[1 1] [2 2]] [3 3]] [4 4]] 445
    [[[[3 0] [5 3]] [4 4]] [5 5]] 791
    [[[[5 0] [7 4]] [5 5]] [6 6]] 1137
    [[[[8 7] [7 7]] [[8 6] [7 7]]] [[[0 7] [6 6]] [8 7]]] 3488))

(comment
  (clojure.test/run-tests))