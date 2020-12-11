(ns advent-of-code.2020.day11)

(def matrix {:data    ["00" "01" "02" "10" "11" "12" "20" "21" "22"]
             :rows    3
             :columns 3})

(defn coordinates->position
  [matrix row col]
  (+ (* (:rows matrix) row)
     col))

(defn matrix-get
  [{:keys [data] :as matrix} row col]
  (nth data (coordinates->position matrix row col)))

(comment
  (matrix-get matrix 1 1))