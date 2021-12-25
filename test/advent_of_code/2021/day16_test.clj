(ns advent-of-code.2021.day16-test
  (:require [advent-of-code.2021.day16 :as day16]
            [clojure.test :refer [deftest are is testing]]
            [matcher-combinators.test :refer [match?]]))

(deftest hex->binary-test
  (are [hex binary]
    (match? binary (day16/hex->binary hex))
    \0 [\0 \0 \0 \0]
    \1 [\0 \0 \0 \1]
    \2 [\0 \0 \1 \0]
    \3 [\0 \0 \1 \1]
    \4 [\0 \1 \0 \0]
    \5 [\0 \1 \0 \1]
    \6 [\0 \1 \1 \0]
    \7 [\0 \1 \1 \1]
    \8 [\1 \0 \0 \0]
    \9 [\1 \0 \0 \1]
    \A [\1 \0 \1 \0]
    \B [\1 \0 \1 \1]
    \C [\1 \1 \0 \0]
    \D [\1 \1 \0 \1]
    \E [\1 \1 \1 \0]
    \F [\1 \1 \1 \1]))

(deftest bit->numeric-value-test
  (are [bit number]
    (match? number (day16/bit->numeric-value bit))
    \0 0
    \1 1))

(deftest parse-input-test
  (are [hex binary]
    (match? binary (day16/parse-input [hex]))
    "0" #:bit-seq {:bits [\0 \0 \0 \0]
                   :consumed 0}
    "A2" #:bit-seq {:bits [\1 \0 \1 \0 \0 \0 \1 \0]
                    :consumed 0}
    "0B0" #:bit-seq {:bits [\0 \0 \0 \0 \1 \0 \1 \1 \0 \0 \0 \0]
                     :consumed 0}))

(deftest read-bits-test
  (is (match? [0 #:bit-seq {:bits [\1 \1 \1]
                            :consumed 0}]
              (day16/read-bits 0 #:bit-seq {:bits [\1 \1 \1]
                                            :consumed 0})))

  (is (match? [7 #:bit-seq {:bits empty?
                            :consumed 3}] 
              (day16/read-bits 3 #:bit-seq {:bits [\1 \1 \1]
                                            :consumed 0})))
  
  (is (match? [3 #:bit-seq {:bits [\1 \0 \0]
                            :consumed 3}]
              (day16/read-bits 3 #:bit-seq {:bits [\0 \1 \1 \1 \0 \0]
                                            :consumed 0}))))

(deftest read-raw-bits-test
  (is (match? [empty? #:bit-seq {:bits [\1 \1 \1]
                                 :consumed 0}]
              (day16/read-raw-bits 0 #:bit-seq {:bits [\1 \1 \1]
                                                :consumed 0})))

  (is (match? [[\1 \1 \1] #:bit-seq {:bits empty?
                                     :consumed 3}]
              (day16/read-raw-bits 3 #:bit-seq {:bits [\1 \1 \1]
                                                :consumed 0})))

  (is (match? [[\0 \1 \1] #:bit-seq {:bits [\1 \0 \0]
                                     :consumed 3}]
              (day16/read-raw-bits 3 #:bit-seq {:bits [\0 \1 \1 \1 \0 \0]
                                                :consumed 0}))))

(deftest drop-bits-test
  (is (match? #:bit-seq {:bits [\1 \1 \1]
                         :consumed 0}
              (day16/drop-bits 0 #:bit-seq {:bits [\1 \1 \1]
                                            :consumed 0})))

  (is (match? #:bit-seq {:bits empty?
                         :consumed 3}
              (day16/drop-bits 3 #:bit-seq {:bits [\1 \1 \1]
                                            :consumed 0})))

  (is (match? #:bit-seq {:bits [\1 \0 \0]
                         :consumed 3}
              (day16/drop-bits 3 #:bit-seq {:bits [\0 \1 \1 \1 \0 \0]
                                                :consumed 0}))))

(deftest decode-package-header-test
  (is (match? [#:package {:version 1
                          :type 0}
               #:bit-seq {:bits [\1 \1 \0 \0]
                          :consumed 6}]
              (day16/decode-package-header #:bit-seq {:bits [\0 \0 \1
                                                             \0 \0 \0
                                                             \1 \1 \0 \0]
                                                      :consumed 0}))))

(deftest decode-numeric-literal-test
  (is (match? [2021 #:bit-seq {:bits [\0 \0 \1 \0]
                               :consumed 15}]
              (day16/decode-numeric-literal #:bit-seq {:bits [\1 \0 \1 \1 \1
                                                              \1 \1 \1 \1 \0
                                                              \0 \0 \1 \0 \1
                                                              \0 \0 \1 \0]
                                                       :consumed 0}))))

(comment
  (clojure.test/run-tests))