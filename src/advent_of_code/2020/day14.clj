(ns advent-of-code.2020.day14
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as str]))

(def initial-state {:mask   {:and-mask (utils/parse-int "111111111111111111111111111111111111" 2)
                             :or-mask  0}
                    :memory {}})

(defn run-set-value-mask-command
  [state command]
  (assoc state :mask (select-keys command [:and-mask :or-mask])))

(defn run-write-masked-value-command
  [{{:keys [and-mask or-mask]} :mask :as state} {:keys [address value]}]
  (let [masked-value (-> value
                         (numbers/bitwise-and and-mask)
                         (numbers/bitwise-or or-mask))]
    (assoc-in state [:memory address] masked-value)))

(defn run-set-address-decoder-mask-command
  [state command]
  (assoc state :mask (select-keys command [:base-mask :floating-mask])))

(defn unroll-floating-masks*
  [[ch & rest] power addresses]
  (cond
    (nil? ch)
    addresses
    
    (#{\0 \1} ch)
    (recur rest (dec power) addresses)
    
    :else
    (let [mask (numbers/pow 2 power)
          new-addresses (->> addresses
                             (map (fn [address]
                                    [address (numbers/bitwise-or address mask)]))
                             flatten)]
      (lazy-seq
       (unroll-floating-masks* rest (dec power) new-addresses)))))

(defn unroll-floating-masks
  [base-address floating-mask]
  (let [zero-all-exes-mask (-> (map #(cond
                                        (#{\0 \1} %) \1
                                        :else \0)
                                     floating-mask)
                               str/join
                               (utils/parse-int 2))
        base-masked-address (numbers/bitwise-and zero-all-exes-mask base-address)]
    (unroll-floating-masks* floating-mask 35 [base-masked-address])))

(defn run-write-value-to-decoded-memory
  [{{:keys [base-mask floating-mask]} :mask :as state} {:keys [address value]}]
  (let [addresses (-> address
                      (numbers/bitwise-or base-mask)
                      (unroll-floating-masks floating-mask))]
    (reduce (fn [state address]
              (assoc-in state [:memory address] value))
            state
            addresses)))

(def command-type->command-runner
  {:set-value-mask                run-set-value-mask-command
   :write-masked-value            run-write-masked-value-command
   :set-address-decoder-mask      run-set-address-decoder-mask-command
   :write-value-to-decoded-memory run-write-value-to-decoded-memory})

(defn state-after-execution
  [initial-state commands]
  (reduce (fn [state {:keys [type] :as command}]
            (let [runner (command-type->command-runner type)]
              (runner state command)))
          initial-state
          commands))

(defn parse-set-value-mask-command
  [line]
  (when-let [[_ mask] (re-find #"mask = ([X10]{36})" line)]
    {:type     :set-value-mask
     :and-mask (-> mask (str/replace "X" "1") (utils/parse-int 2))
     :or-mask  (-> mask (str/replace "X" "0") (utils/parse-int 2))}))

(defn parse-write-masked-value
  [line]
  (when-let [[_ address value] (re-find #"mem\[(\d+)\] = (\d+)" line)]
    {:type    :write-masked-value
     :address (utils/parse-int address)
     :value   (utils/parse-int value)}))

(defn parse-set-address-decoder-mask-command
  [line]
  (when-let [[_ mask] (re-find #"mask = ([X10]{36})" line)]
    {:type          :set-address-decoder-mask
     :base-mask     (-> mask (str/replace "X" "0") (utils/parse-int 2))
     :floating-mask mask}))

(defn parse-write-value-to-decoded-memory
  [line]
  (when-let [[_ address value] (re-find #"mem\[(\d+)\] = (\d+)" line)]
    {:type    :write-value-to-decoded-memory
     :address (utils/parse-int address)
     :value   (utils/parse-int value)}))

(defn parse-command
  [parsers line]
  (some #(% line) parsers))

(defn part1-solution
  [lines]
  (let [parsers [parse-write-masked-value
                 parse-set-value-mask-command]
        commands (map (partial parse-command parsers) lines)]
    (->> commands
         (state-after-execution initial-state)
         :memory
         vals
         numbers/sum)))

(defn part2-solution
  [lines]
  (let [parsers [parse-write-value-to-decoded-memory
                 parse-set-address-decoder-mask-command]
        commands (map (partial parse-command parsers) lines)]
    (->> commands
         (state-after-execution initial-state)
         :memory
         vals
         numbers/sum)))

(defn day-solution
  []
  (utils/with-lines "2020/day14.txt"
    (fn [lines]
      (utils/tap (part1-solution lines))
      (utils/tap (part2-solution lines))))
  nil)

(comment
  (let [{:keys [and-mask or-mask]} (parse-set-mask-command "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")]
    (bit-or or-mask (bit-and and-mask 11)))
  (class (parse-set-address-decoder-mask-command "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"))

  (unroll-floating-masks "abc")
  (day-solution))
