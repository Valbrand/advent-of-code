(ns advent-of-code.utils-test
  (:require [advent-of-code.utils :as utils]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]))

(deftest surrounding-matrix-indices
  (is (match? [[0 1] [1 0] [1 1]]
              (utils/surrounding-matrix-indices [0 0] [[0 0 0]
                                                       [0 0 0]
                                                       [0 0 0]]))))

(comment
  (clojure.test/run-tests))