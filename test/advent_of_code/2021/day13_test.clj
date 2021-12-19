(ns advent-of-code.2021.day13-test
  (:require [advent-of-code.2021.day13 :as day13]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]))

(def test-transparent-paper
  {:coordinates (apply sorted-set #{[0N 3N]
                                    [0N 13N]
                                    [0N 14N]
                                    [1N 10N]
                                    [2N 14N]
                                    [3N 0N]
                                    [3N 4N]
                                    [4N 1N]
                                    [4N 11N]
                                    [6N 0N]
                                    [6N 10N]
                                    [6N 12N]
                                    [8N 4N]
                                    [8N 10N]
                                    [9N 0N]
                                    [9N 10N]
                                    [10N 4N]
                                    [10N 12N]})
   :folds [["y" 7N] ["x" 5N]]})

(deftest parse-input
  (is (match? test-transparent-paper
              (day13/parse-input ["6,10"
                                  "0,14"
                                  "9,10"
                                  "0,3"
                                  "10,4"
                                  "4,11"
                                  "6,0"
                                  "6,12"
                                  "4,1"
                                  "0,13"
                                  "10,12"
                                  "3,4"
                                  "3,0"
                                  "8,4"
                                  "1,10"
                                  "2,14"
                                  "8,10"
                                  "9,0"
                                  ""
                                  "fold along y=7"
                                  "fold along x=5"]))))


(deftest fold-along-x-axis
  (comment
    "y x0123     y x01
     0  #.|.# -> 0  #..
     1  #.|##    1  ##")
  (is (match? #{[0 0] [0 1] [1 1]}
              (day13/fold-along-x-axis 2 (into (sorted-set)
                                               #{[0 0] [0 1] [3 1] [4 1] [4 0]}))))

  (comment
    "y x0123    y x01
     0  #.|. -> 0  #..
     1  #.|#    1  ##")
  (is (match? #{[0 0] [0 1] [1 1]}
              (day13/fold-along-x-axis 2 (into (sorted-set)
                                               #{[0 0] [0 1] [3 1]}))))
  
  (comment
    "y x012345    y x012
     0  ..|..# -> 0  #..
     1  .#|#.#    1  #.#")
  (is (match? #{[0 0] [0 1] [2 1]}
              (day13/fold-along-x-axis 2 (into (sorted-set)
                                               #{[1 1] [3 1] [5 1] [5 0]}))))

  (comment
    "y x012345678910    y x01234
     0  #.##.|#..#.     0  #####
     1  #...#|.....     1  #...#
     2  .....|#...#     2  #...#
     3  #...#|.....  -> 3  #...#
     4  .#.#.|#.###     4  #####")
  (is (match? #{[0 0] [0 1] [0 2] [0 3] [0 4]
                [1 0] [1 4]
                [2 0] [2 4]
                [3 0] [3 4]
                [4 0] [4 1] [4 2] [4 3] [4 4]}
                (day13/fold-along-x-axis 5 (into (sorted-set)
                                                 #{[0 0] [0 1] [0 3]
                                                   [1 4]
                                                   [2 0]
                                                   [3 0] [3 4]
                                                   [4 1] [4 3]
                                                   [6 0] [6 2] [6 4]
                                                   [8 4]
                                                   [9 0] [9 4]
                                                   [10 2] [10 4]})))))

(deftest fold-along-y-axis
  (comment
    "y x0123  y x01
     0  .#    0  ##
     1  ..    1  .#
     2  -- -> 2  .#
     3  .#
     4  ..
     5  ##")
  (is (match? #{[0 0] [1 0] [1 1] [1 2]}
              (day13/fold-along-y-axis 2 (into (sorted-set)
                                               #{[0 5] [1 0] [1 3] [1 5]}))))

  (comment
    "y x0123  y x01
     0  ##    0  ##
     1  ..    1  .#
     2  -- -> 
     3  .#")
  (is (match? #{[0 0] [1 0] [1 1]}
              (day13/fold-along-y-axis 2 (into (sorted-set)
                                               #{[0 0] [1 0] [1 3]}))))

  (comment
    "y x0123  y x01
     0  ##    0  ##
     1  ..    1  .#
     2  -- -> 
     3  .#
     4  ##")
  (is (match? #{[0 0] [1 0] [1 1]}
              (day13/fold-along-y-axis 2 (into (sorted-set)
                                               #{[0 0] [0 4] [1 0] [1 3] [1 4]})))))

(deftest dots-string
  (is (match? (string/trim "
.#.
#.#
.#.")
              (day13/dots-string (into (sorted-set) #{[0 1] [1 0] [1 2] [2 1]})))))

(comment
  (clojure.test/run-tests))