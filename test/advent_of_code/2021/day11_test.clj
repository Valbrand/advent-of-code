(ns advent-of-code.2021.day11-test
  (:require [advent-of-code.2021.day11 :as day11]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]))

(deftest state-after-energy-increment
  (is (match? {:energy-map (repeat 10 (repeat 10 1))
               :flash-positions #{}}
              (day11/state-after-energy-increment {:energy-map (repeat 10 (repeat 10 0))})))
  
  (is (match? {:energy-map [[10 1 1 1 1 1 1 1 1 1]
                            [1 1 10 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]]
               :flash-positions #{[0 0] [1 2]}
               :flashes 2}
              (day11/state-after-energy-increment {:energy-map [[9 0 0 0 0 0 0 0 0 0]
                                                                [0 0 9 0 0 0 0 0 0 0]
                                                                [0 0 0 0 0 0 0 0 0 0]
                                                                [0 0 0 0 0 0 0 0 0 0]
                                                                [0 0 0 0 0 0 0 0 0 0]
                                                                [0 0 0 0 0 0 0 0 0 0]
                                                                [0 0 0 0 0 0 0 0 0 0]
                                                                [0 0 0 0 0 0 0 0 0 0]
                                                                [0 0 0 0 0 0 0 0 0 0]
                                                                [0 0 0 0 0 0 0 0 0 0]]}))))

(deftest state-after-step
  (is (match? {:energy-map (repeat 10 (repeat 10 1))
               :flashes 0}
              (day11/state-after-step {:energy-map (repeat 10 (repeat 10 0))})))

  (is (match? {:energy-map [[0 3 2 2 1 1 1 1 1 1]
                            [2 3 0 2 1 1 1 1 1 1]
                            [1 2 2 2 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]]
               :flashes 2}
              (day11/state-after-step {:energy-map [[9 0 0 0 0 0 0 0 0 0]
                                                    [0 0 9 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]]})))
  
  (is (match? {:energy-map [[0 0 4 2 1 1 1 1 1 1]
                            [4 0 0 2 1 1 1 1 1 1]
                            [2 3 3 2 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]
                            [1 1 1 1 1 1 1 1 1 1]]
               :flashes 4}
              (day11/state-after-step {:energy-map [[9 6 0 0 0 0 0 0 0 0]
                                                    [0 7 9 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]
                                                    [0 0 0 0 0 0 0 0 0 0]]}))))



(comment
  (clojure.test/run-tests))