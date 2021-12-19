(ns advent-of-code.2021.day12-test
  (:require [advent-of-code.2021.day12 :as day12]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]))

(deftest parse-input
  (is (match? {"a" {:small? true
                    :edges #{"B"}}
               "B" {:small? false
                    :edges #{"a"}}}
              (day12/parse-input ["a-B"]))))

(comment
  (clojure.test/run-tests))