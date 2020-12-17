(ns advent-of-code.2020.day8
  (:require [advent-of-code.utils :as utils]))

(def command-pattern #"^(nop|acc|jmp) ((?:\+|-)\d+)")

(defn parse-command
  [line]
  (let [[_ command argument] (re-find command-pattern line)]
    [command (utils/parse-int argument)]))

(defn run-with-loop-detection
  [commands]
  (loop [accumulator 0
         command-idx 0
         indices-executed #{}]
    (let [[command arg] (get commands command-idx ["out-of-bounds"])]
      (cond
        (= command-idx (count commands))
        {:result       :terminated
         :accumulator  accumulator
         :instructions indices-executed}

        (contains? indices-executed command-idx)
        {:result       :loop-detected
         :accumulator  accumulator
         :instructions indices-executed}
        
        (= command "out-of-bounds")
        {:result       :crashed
         :accumulator  accumulator
         :instructions indices-executed}

        (= command "nop")
        (recur accumulator
               (inc command-idx)
               (conj indices-executed command-idx))

        (= command "acc")
        (recur (+ accumulator arg)
               (inc command-idx)
               (conj indices-executed command-idx))

        (= command "jmp")
        (recur accumulator
               (+ command-idx arg)
               (conj indices-executed command-idx))))))

(defn part1-solution
  [lines]
  (->> lines
       (mapv parse-command)
       run-with-loop-detection
       :accumulator))

(defn fix-infinite-loop
  [commands]
  (let [modifiable-commands (->> commands
                                 run-with-loop-detection
                                 :instructions
                                 (filter #(#{"nop" "jmp"} (get-in commands [% 0]))))
        command-exchange-map {"nop" "jmp"
                              "jmp" "nop"}]
    (loop [[command-to-modify & rest] modifiable-commands]
      (when (nil? command-to-modify)
        (throw (ex-info "Backtracking could not find a solution" {})))

      (let [modified-commands (some-> commands (update command-to-modify update 0 command-exchange-map))
            {:keys [result accumulator]} (run-with-loop-detection modified-commands)]
        (case result
          :terminated
          accumulator
          
          :crashed
          (recur rest)
          
          :loop-detected
          (recur rest))))))

(defn part2-solution
  [lines]
  (->> lines
       (mapv parse-command)
       fix-infinite-loop))

(defn day-solution
  []
  (utils/with-lines "2020/day8.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))
      (utils/tap (part2-solution lines))))
  nil)

(comment
  (day-solution))
