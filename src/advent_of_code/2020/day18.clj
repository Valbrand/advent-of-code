(ns advent-of-code.2020.day18
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.walk]))

(defmacro infix
  ([arg]
   (if (and (list? arg)
            (or (number? (first arg))
                (list? (first arg))))
     (list* 'infix arg)
     arg))
  ([a op b & rest]
   (list* 'infix (list op (list 'infix a) (list 'infix b)) rest)))

(defn parse-line
  [line]
  (->> line
       (format "(infix %s)")
       read-string
       eval))

(defn part1-solution
  [lines]
  (->> lines
       (map parse-line)
       numbers/sum))

(defmacro infix-with-plus-precedence
  ([arg]
   (if (and (list? arg)
            (or (number? (first arg))
                (list? (first arg))))
     (list* 'infix-with-plus-precedence arg)
     arg))
  ([a op b]
   (list op
         (list 'infix-with-plus-precedence a)
         (list 'infix-with-plus-precedence b)))
  ([a op1 b op2 c & rest]
   (cond
     (= op1 '+)
     (list* 'infix-with-plus-precedence
            (list 'infix-with-plus-precedence a op1 b)
            op2
            c
            rest)

     (= op2 '+)
     (list* 'infix-with-plus-precedence
            a
            op1
            (list 'infix-with-plus-precedence b op2 c)
            rest)
     
     :else
     (list 'infix-with-plus-precedence
           a
           op1
           (list* 'infix-with-plus-precedence b op2 c rest)))))

(defn parse-line-with-plus-precedence
  [line]
  (->> line
       (format "(infix-with-plus-precedence %s)")
       read-string
       eval))

(defn part2-solution
  [lines]
  (->> lines
       (map parse-line-with-plus-precedence)
       numbers/sum))

(defn day-solution
  []
  (utils/with-lines "2020/day18.txt"
    (fn [lines]
      #tap (part1-solution lines)
      #tap (part2-solution lines)))
  nil)

(comment
  (day-solution)
  
  ;; For debugging macroexpansion
  (let [test-expression '(infix-with-plus-precedence 1 + 2 * 3 + 4 + 5 * 7)]
    (->> (range)
         (reductions (fn [previous-expansion _]
                       (let [current-expansion (clojure.walk/postwalk macroexpand previous-expansion)]
                         (if (= previous-expansion current-expansion)
                           (reduced current-expansion)
                           current-expansion)))
                     (macroexpand test-expression))
         ;; preventing infinite loops
         ;; 10 runs is supposed to be more than necessary, but it can be increased at will
         (take 10)
         last)))