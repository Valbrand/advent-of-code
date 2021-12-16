(ns advent-of-code.2021.day8-test
  (:require [advent-of-code.2021.day8 :as day8]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]))

(def test-display
  {:patterns [#{\a \b \c \d \e \f \g} #{\b \c \d \e \f} #{\a \c \d \f \g} #{\a \b \c \d \f} #{\a \b \d} #{\a \b \c \d \e \f} #{\b \c \d \e \f \g} #{\a \b \e \f} #{\a \b \c \d \e \g} #{\a \b}]
   :digits [#{\b \c \d \e \f} #{\a \b \c \d \f} #{\b \c \d \e \f} #{\a \b \c \d \f}]})

(deftest a-segment-matcher
  (is (match? \d
              ((day8/segment->segment-matcher \a) test-display)))

  (is (match? \e
              ((day8/segment->segment-matcher \b) test-display)))
  
  (is (match? \f
              ((day8/segment->segment-matcher \d) test-display))))

(deftest number->pattern-matcher
  (is (match? #{\c \a \g \e \d \b}
              ((day8/number->pattern-matcher 0) test-display)))

  (is (match? #{\a \b}
              ((day8/number->pattern-matcher 1) test-display)))
  
  (is (match? #{\g \c \d \f \a}
              ((day8/number->pattern-matcher 2) test-display)))

  (is (match? #{\f \c \a \d \b}
              ((day8/number->pattern-matcher 3) test-display)))

  (is (match? #{\a \b \e \f}
              ((day8/number->pattern-matcher 4) test-display)))
  
  (is (match? #{\c \d \f \b \e}
              ((day8/number->pattern-matcher 5) test-display)))
  
  (is (match? #{\c \d \f \g \e \b}
              ((day8/number->pattern-matcher 6) test-display)))

  (is (match? #{\a \b \d}
              ((day8/number->pattern-matcher 7) test-display)))

  (is (match? #{\a \b \c \d \e \f \g}
              ((day8/number->pattern-matcher 8) test-display)))
  
  (is (match? #{\c \e \f \a \d \b}
              ((day8/number->pattern-matcher 9) test-display))))

(comment
  (clojure.test/run-tests))