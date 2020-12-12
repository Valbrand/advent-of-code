(ns advent-of-code.2020.day12
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]))

(def initial-state
  {:facing      :east
   :ew-distance 0N
   :ns-distance 0N})

(def direction->distance-delta-op
  {:east  [:ew-distance +]
   :west  [:ew-distance -]
   :north [:ns-distance +]
   :south [:ns-distance -]})

(defn go-forward
  [state command]
  (let [to-move (:argument command)
        [distance-attr movement-op] (get direction->distance-delta-op (:facing state))]
    (update state distance-attr movement-op to-move)))

(def command->absolute-movement-direction
  {\N :north
   \S :south
   \E :east
   \W :west})

(defn do-absolute-movement
  [state command]
  (let [to-move (:argument command)
        direction-to-move (get command->absolute-movement-direction (:command command))
        [distance-attr movement-op] (get direction->distance-delta-op direction-to-move)]
    (update state distance-attr movement-op to-move)))

(def directions-clockwise [:north :east :south :west])
(def directions-counterclockwise (rseq directions-clockwise))

(defn find-index
  [item coll]
  (->> (range (count coll))
       (filter #(= item (nth coll %)))
       first))

(defn turn
  [directions-order state command]
  (when (pos? (mod (:argument command) 90N))
    (throw (ex-info (str "Turn argument not a multiple of 90: " (:argument command)) {})))
  (let [current-facing-direction (:facing state)
        current-facing-direction-idx (find-index current-facing-direction directions-order)
        times-to-turn (quot (:argument command) 90N)
        new-facing-idx (mod (+ current-facing-direction-idx times-to-turn)
                            (count directions-order))]
    (assoc state :facing (nth directions-order new-facing-idx))))

(def ^{:arglists '([state command])} turn-right (partial turn directions-clockwise))
(def ^{:arglists '([state command])} turn-left (partial turn directions-counterclockwise))

(defn command->command-fn
  [{:keys [command]}]
  (cond
    (#{\N \S \E \W} command)
    do-absolute-movement
    
    (= command \L)
    turn-left
    
    (= command \R)
    turn-right
    
    (= command \F)
    go-forward))

(defn execute-commands
  [command->command-fn state commands]
  (reduce (fn [state command]
            (let [command-fn (command->command-fn command)]
              (command-fn state command)))
          state
          commands))

(defn part1-solution
  [commands]
  (let [{:keys [ew-distance ns-distance]} (execute-commands command->command-fn initial-state commands)]
    (+ (numbers/abs ew-distance)
       (numbers/abs ns-distance))))

(def initial-state-with-waypoint
  {:waypoint    {:ew-distance 10N
                 :ns-distance 1N}
   :ew-distance 0N
   :ns-distance 0N})

(defn go-forward-to-waypoint
  [state command]
  (let [to-move (:argument command)
        waypoint (:waypoint state)
        ew-to-move (* to-move (:ew-distance waypoint))
        ns-to-move (* to-move (:ns-distance waypoint))]
    (-> state
        (update :ew-distance + ew-to-move)
        (update :ns-distance + ns-to-move))))

(defn move-waypoint
  [state command]
  (let [to-move (:argument command)
        direction-to-move (get command->absolute-movement-direction (:command command))
        [distance-attr movement-op] (get direction->distance-delta-op direction-to-move)]
    (update-in state [:waypoint distance-attr] movement-op to-move)))

(defn waypoint-distance->direction
  [{:keys [ew-distance ns-distance]}]
  [[(if (pos? ew-distance) :east :west)
    (numbers/abs ew-distance)]
   [(if (pos? ns-distance) :north :south)
    (numbers/abs ns-distance)]])

(defn waypoint-directions->distance
  [distances]
  (into {}
        (map (fn [[direction value]]
               (let [[direction-attr signal-op] (direction->distance-delta-op direction)]
                 [direction-attr (signal-op (numbers/abs value))])))
        distances))

(defn rotate-waypoint
  [directions-order state command]
  (when (pos? (mod (:argument command) 90N))
    (throw (ex-info (str "Turn argument not a multiple of 90: " (:argument command)) {})))
  (let [times-to-turn (quot (:argument command) 90N)
        current-waypoint-directions (waypoint-distance->direction (:waypoint state))
        new-waypoint-directions (->> current-waypoint-directions
                                     (map (fn [[direction value]]
                                            (let [direction-idx (find-index direction directions-order)
                                                  new-direction-idx (mod (+ direction-idx times-to-turn)
                                                                         (count directions-order))]
                                              [(nth directions-order new-direction-idx) value]))))]
    (assoc state :waypoint (waypoint-directions->distance new-waypoint-directions))))

(def ^{:arglists '([state command])} rotate-waypoint-right (partial rotate-waypoint directions-clockwise))
(def ^{:arglists '([state command])} rotate-waypoint-left (partial rotate-waypoint directions-counterclockwise))

(defn command->waypoint-command-fn
  [{:keys [command]}]
  (cond
    (#{\N \S \E \W} command)
    move-waypoint

    (= command \L)
    rotate-waypoint-left

    (= command \R)
    rotate-waypoint-right

    (= command \F)
    go-forward-to-waypoint))

(defn part2-solution
  [commands]
  (let [{:keys [ew-distance ns-distance]} (execute-commands command->waypoint-command-fn initial-state-with-waypoint commands)]
    (+ (numbers/abs ew-distance)
       (numbers/abs ns-distance))))

(def command-pattern #"^([NSEWLRF])(\d+)$")

(defn parse-command
  [line]
  (let [[_ [command] arg] (re-find command-pattern line)]
    {:command  command
     :argument (utils/parse-int arg)}))

(defn parse-commands
  [lines]
  (map parse-command lines))

(defn day-solution
  []
  (utils/with-lines "2020/day12.txt"
    (fn [lines]
      (let [commands (parse-commands lines)]
        (utils/tap (part1-solution commands))
        (utils/tap (part2-solution commands)))))
  nil)

(comment
  (day-solution))