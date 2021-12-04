(ns advent-of-code.2021.day4
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]))

(defn parse-board
  [board-lines]
  (->> board-lines
       (utils/zip (range))
       (reduce (fn [board [row-index line]]
                 (->> (utils/parse-integers-line line #" +")
                      (utils/zip (range))
                      (reduce (fn [board [col-index num]]
                                (-> board
                                    (assoc-in [:num->pos num] [row-index col-index])
                                    (assoc-in [:pos->num [row-index col-index]] num)))
                              board)))
               {:num->pos {}
                :pos->num {}})))

(defn boards
  [lines-without-numbers-to-draw]
  (into []
        (comp (partition-by empty?)
              (remove #(every? empty? %))
              (map parse-board))
        lines-without-numbers-to-draw))

(defn parse-input
  [lines]
  {:numbers-to-draw (-> lines first (utils/parse-integers-line #","))
   :drawn-numbers #{}
   :last-drawn-number nil
   :boards (boards (rest lines))})

(defn row-numbers
  [row-index {:keys [pos->num]}]
  (into []
        (comp (map #(get pos->num %))
              (take-while some?))
        (utils/zip (repeat row-index) (range))))

(defn column-numbers
  [col-index {:keys [pos->num]}]
  (into []
        (comp (map #(get pos->num %))
              (take-while some?))
        (utils/zip (range) (repeat col-index))))

(defn win?
  [row-or-col {:keys [drawn-numbers]}]
  (= (count row-or-col)
     (count (filter #(contains? drawn-numbers %) row-or-col))))

(defn winner
  [{:keys [last-drawn-number] :as game} board]
  (when-let [[drawn-pos-row drawn-pos-col] (-> board :num->pos (get last-drawn-number))]
    (let [row (row-numbers drawn-pos-row board)
          column (column-numbers drawn-pos-col board)]
      (cond
        (win? row game)
        (assoc board :winning-sequence row)

        (win? column game)
        (assoc board :winning-sequence column)))))

(defn game-end-state
  [game]
  (when-let [winners (seq (keep (fn [[index board]]
                                  (when-let [winner-board (winner game board)]
                                    [index winner-board]))
                                (utils/zip (range) (:boards game))))]
    (reduce (fn [game [index winner]]
              (-> game
                  (assoc-in [:boards index] winner)
                  (update :winners conj winner)))
            (assoc game :winners [])
            winners)))

(defn draw-number
  [{:keys [numbers-to-draw] :as game}]
  (let [drawn-number (first numbers-to-draw)]
    (-> game
        (assoc :last-drawn-number drawn-number)
        (update :numbers-to-draw rest)
        (update :drawn-numbers conj drawn-number))))

(defn game-with-first-winner
  [game]
  (->> (iterate draw-number game)
       (drop 1)
       (some game-end-state)))

(defn unmarked-numbers
  [{:keys [drawn-numbers]} {:keys [num->pos]}]
  (->> (keys num->pos)
       (remove #(contains? drawn-numbers %))))

(defn winner-score
  [{:keys [winners last-drawn-number] :as game}]
  (* (numbers/sum (unmarked-numbers game (first winners)))
     last-drawn-number))

(defn part1-solution
  []
  (let [path "2021/day4.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             parse-input
             game-with-first-winner
             winner-score)))))

(defn remove-winning-boards
  [game]
  (update game
          :boards
          (fn [boards]
            (into []
                  (remove #(contains? % :winning-sequence))
                  boards))))

(defn end-game-state?
  [game]
  (some? (:winners game)))

(defn game-with-last-winner
  [game]
  (first (sequence (comp (drop 1)
                         (filter end-game-state?)
                         (drop-while #(seq (:boards %)))
                         (take 1)) ;; iterate will hang looking for the rest of the seq otherwise
                   (iterate (fn [game]
                              (let [next-game-state (draw-number game)]
                                (or (some-> next-game-state
                                            game-end-state
                                            remove-winning-boards)
                                    (dissoc next-game-state :winners))))
                            game))))

(defn part2-solution
  []
  (let [path "2021/day4.txt"]
    (utils/with-lines path
      (fn [lines]
        (->> lines
             parse-input
             game-with-last-winner
             winner-score)))))

(comment
  (part1-solution)
  (part2-solution)

  *e)