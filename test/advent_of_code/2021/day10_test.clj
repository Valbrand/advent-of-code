(ns advent-of-code.2021.day10-test
  (:require [advent-of-code.2021.day10 :as day10]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]))

(deftest analyze-line
  (is (match? {:status :ok
               :stack  []}
              (day10/analyze-line "[<>({}){}[([])<>]]")))
  
  (is (match? {:status :ok
               :stack  [\]]}
              (day10/analyze-line "[<>({}){}[([])<>]")))
  
  (is (match? {:status :ok
               :stack  [\] \)]}
              (day10/analyze-line "[[]()(<>")))
  
  (is (match? {:status :corrupted
               :violation \>}
              (day10/analyze-line "[<>({}){}[([]><>]]"))))

(deftest autocompletion-score-for-line
  (is (match? 294
              (day10/autocompletion-score-for-line {:status :ok
                                                    :stack [\> \} \) \]]})))
  
  (is (match? 995444
              (day10/autocompletion-score-for-line {:status :ok
                                                    :stack [\> \} \] \} \] \} \} \] \]]})))
  
  (is (match? 5566
              (day10/autocompletion-score-for-line {:status :ok
                                                    :stack [\) \} \] \> \} \)]}))))

(deftest autocompletion-score-for-program
  (is (match? 5566
              (day10/autocompletion-score-for-program
               [{:status :ok
                 :stack [\> \} \) \]]}
                {:status :ok
                 :stack [\> \} \] \} \] \} \} \] \]]}
                {:status :corrupted
                 :violation \]}
                {:status :ok
                 :stack [\) \} \] \> \} \)]}]))))

(comment
  (clojure.test/run-tests))