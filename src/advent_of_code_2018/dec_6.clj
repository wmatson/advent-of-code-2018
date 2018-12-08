(ns advent-of-code-2018.dec-6
  (:require [advent-of-code-2018.core :as ac]
            [medley.core :as medley]))

(defn- get-input []
  (->> (ac/get-input-lines "dec-6.txt")
       (map #(clojure.string/split % #", "))
       (map (fn [coords] (mapv #(Integer/parseInt %) coords)))))

(def ^:private sample-input
  [[1 1]
   [1 6]
   [8 3]
   [3 4]
   [5 5]
   [8 9]])

(defn- bordered-claim-range [input]
  (let [range-ends (juxt min max)
        [min-x max-x] (apply range-ends (map first input))
        [min-y max-y] (apply range-ends (map second input))]
    {:min-x      min-x
     :min-y      min-y
     :max-x      max-x
     :max-y      max-y
     :all-coords (for [x (range min-x (inc max-x))
                       y (range min-y (inc max-y))]
                   [x y])}))

(defn- get-borderers [input {:keys [min-x max-x min-y max-y] :as bordered-claim-range}]
  (filter (fn [[x y]]
            (or (= x min-x) (= y min-y)
                (= x max-x) (= y max-y)))
          input))

(defn- manhattan-distance [left right]
  (->> (map - left right)
       (map medley/abs)
       (apply +)))

(defn- nearest [point nodes]
  (let [distances (map manhattan-distance (repeat point) nodes)
        min-dist (apply min distances)]
    (if (> (count (filter #{min-dist} distances)) 1)
      :tied-claim
      (get (zipmap distances nodes) min-dist))))

(defn- count-claims [nodes all-coords]
  (frequencies (map #(nearest % nodes) all-coords)))

(defn bordered-claims [input]
  (let [claim-range (bordered-claim-range input)
        borderers (get-borderers input claim-range)]
    (->> (count-claims input (:all-coords claim-range))
         (medley/remove-keys (conj (set borderers) :tied-claim)))))

(defn solve-1 []
  (->> (get-input)
       (bordered-claims)
       vals
       (apply max)))

(defn- total-distance [nodes point]
  (apply + (map manhattan-distance (repeat point) nodes)))

(defn solve-2 []
  (let [nodes (get-input)]
    (->> nodes
         bordered-claim-range
         :all-coords
         (map #(total-distance nodes %))
         (filter #(< % 10000))
         count)))