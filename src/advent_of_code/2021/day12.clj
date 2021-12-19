(ns advent-of-code.2021.day12
  "Passage pathing
   
   Graph problem.
   
   All paths from start to end are required. I'm going to
   represent the graph as an adjacency list and use a DFS
   to find the paths. I don't see a clear benefit between using
   BFS and DFS, so far it's just a personal choice.
   
   A better approach could be applied to part 2.
   It took around 3-3.5 seconds to finish, and it took some
   effort to get the predicate right for part 2. Admittedly,
   I took too long before thinking of implementing tests
   for the part 2 predicate. It would have saved me a lot of time,
   mostly because eventually the bug wasn't in the predicate
   itself but in the arguments I was passing to it."
  (:require [advent-of-code.utils :as utils]
            [advent-of-code.numbers :as numbers]
            [clojure.string :as string]))

(defn small-cave?
  [cave-name]
  (every? #(<= (int \a) (int %) (int \z)) cave-name))

(defn cave-exit?
  [cave-name]
  (= "end" cave-name))

(defn cave-entrance?
  [cave-name]
  (= "start" cave-name))

(defn parse-input
  [lines]
  (->> lines
       (map #(string/split % #"-"))
       (reduce (fn [graph [vertex-a vertex-b]]
                 (-> graph
                     (assoc-in [vertex-a :small?] (small-cave? vertex-a))
                     (assoc-in [vertex-b :small?] (small-cave? vertex-b))
                     (update-in [vertex-a :edges] (fnil conj #{}) vertex-b)
                     (update-in [vertex-b :edges] (fnil conj #{}) vertex-a)))
               {})))

(defn traverse-caves
  [should-visit-node? graph]
  (->> (iterate (fn [{:keys [to-visit]
                      :as traversal-state}]
                  (let [[visited cave-being-visited path-so-far] (peek to-visit)
                        visited (update visited cave-being-visited (fnil inc 0))
                        current-path-in-order (conj path-so-far cave-being-visited)
                        new-traversal-state (update traversal-state :to-visit pop)]
                    (cond
                      (cave-exit? cave-being-visited)
                      (update new-traversal-state
                              :paths-out
                              conj
                              [visited current-path-in-order])

                      :else
                      (update new-traversal-state
                              :to-visit
                              into
                              (->> (get-in graph [cave-being-visited :edges])
                                   (filter #(should-visit-node? graph visited %))
                                   (map #(vector visited % current-path-in-order)))))))
                {:to-visit [[{} "start" []]]
                 :paths-out []})
       (drop-while (comp seq :to-visit))
       first
       :paths-out))

(defn part1-solution
  []
  (utils/with-input
    (fn [lines]
      (->> lines
           parse-input
           (traverse-caves (fn [_graph visited node]
                             (or (not (small-cave? node))
                                 (not (contains? visited node)))))
           count))))

(defn part2-solution
  []
  (utils/with-input
    (fn [lines]
      (->> lines
           parse-input
           (traverse-caves (fn [graph visited node]
                             ;; is it the cave entrance?
                             ;;   if true, return false
                             ;;   if false, is it a large cave?
                             ;;      if true, return true
                             ;;      if false, has it never been visited?
                             ;;        if true, return true
                             ;;        if false, has any small cave ever been visited twice?
                             ;;          if true, return false
                             ;;          if false, return true
                             (let [small-cave-node? #(get-in graph [% :small?])]
                               (and (not (cave-entrance? node))
                                    (or (not (small-cave-node? node))
                                        (zero? (get visited node 0))
                                        (not (some (comp #{2} second)
                                                   (filter (comp small-cave-node? first) visited))))))))
           count))))

(comment
  (time (part1-solution))
  (time (part2-solution))

  *e)
