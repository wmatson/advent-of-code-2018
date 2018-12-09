(ns advent-of-code-2018.dec-7
  (:require [advent-of-code-2018.core :as ac]
            [medley.core :as medley]))

(def sample-input "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.")

(defn parse-input [lines]
  (->> lines
       (map #(re-find #"Step (\w) .* before step (\w)" %))
       (map rest)
       (map #(mapv keyword %))
       (group-by first)
       (medley/map-vals #(into #{} (map second %)))))

(defn get-input []
  (parse-input (ac/get-input-lines "dec-7.txt")))

(defn- next-possible [instructions]
  (apply dissoc instructions (apply clojure.set/union (vals instructions))))

(defn work-step [{:keys [completed instructions]}]
  (let [to-do (-> (next-possible instructions) keys sort first)]
    {:completed    (conj completed to-do)
     :instructions (dissoc instructions to-do)}))

(defn solve-1 [parsed-input]
  (let [start {:completed [] :instructions parsed-input}
        to-last (->> (iterate work-step start)
                     (take-while (comp not-empty :instructions))
                     (last))
        [last-blocker final-steps] (first (:instructions to-last))]
    (apply conj (:completed to-last) last-blocker (sort final-steps))))

(defn- add-last-step-sentinel [instructions]
  (let [all-dests (apply clojure.set/union (vals instructions))
        last-dests (clojure.set/difference all-dests (set (keys instructions)))]
    (into instructions (zipmap last-dests (repeat #{})))))

(defn- time-required [task]
  (-> task name first int
      (- 4)))

(defn- next-possible-parallel [{:keys [instructions completed in-progress]}]
  (let [remaining (apply dissoc instructions completed)
        enqueued (keys in-progress)]
    (apply dissoc remaining (apply clojure.set/union enqueued (vals remaining)))))

(defn- enqueue [{:keys [in-progress] :as world} max-amount]
  (let [available-workers (max 0 (- max-amount (count in-progress)))
        to-enqueue (take available-workers (-> (next-possible-parallel world) keys sort))]
    (-> world
        (update :in-progress into (map (juxt identity time-required) to-enqueue)))))

(defn- work [{:keys [in-progress completed] :as world}]
  (if (empty? in-progress)
    world
    (let [to-next-completion (first (sort (map val in-progress)))
          worked (medley/map-vals #(- % to-next-completion) in-progress)
          done (keys (medley/filter-vals zero? worked))]
      (-> world
          (update :completed #(apply conj % done))
          (assoc :in-progress (medley/remove-vals zero? worked))
          (update :time-elapsed + to-next-completion)))))

(defn- completed? [{:keys [in-progress] :as world}]
  (and (empty? (next-possible-parallel world))
       (empty? in-progress)))

(defn solve-2 [parsed-input]
  (:time-elapsed
    (first
      (drop-while (complement completed?)
                  (iterate #(-> % (enqueue 5) work)
                           {:time-elapsed 0
                            :completed    []
                            :in-progress  {}
                            :instructions (add-last-step-sentinel parsed-input)})))))

(comment
  (->> (iterate #(-> % (enqueue 5) work)
                {:time-elapsed 0
                 :completed    []
                 :in-progress  (sorted-map)
                 :instructions (add-last-step-sentinel parsed-input)})
       (take 30)
       (map #(assoc % :remaining (apply dissoc (:instructions %) (:completed %))))
       (map #(dissoc % :instructions)))
  )