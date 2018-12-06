(ns advent-of-code-2018.dec-3
  (:require [advent-of-code-2018.core :as ac]))

;(ac/get-input-lines "dec-3.txt")
(defn parse-line [line]
  (let [[id left top width height] (->> line
                                     (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
                                     rest
                                     (map #(Integer/parseInt %)))]
    {:top top :left left :width width :height height :id id}))

;(map parse-line (ac/get-input-lines "dec-3.txt"))

(defn line->taken-coords [{:keys [top left width height]}]
  (into #{}
        (for [x (range left (+ left width))
              y (range top (+ top height))]
          [x y])))

;(line->taken-coords {:top 1 :left 2 :width 3 :height 4})

(defn get-multi-claimed [lines]
  (->> lines
       (map line->taken-coords)
       (apply concat)
       (frequencies)
       (filter (comp #(> % 1) second))
       keys))

(defn solve-1 []
  (->> (ac/get-input-lines "dec-3.txt")
       (map parse-line)
       (get-multi-claimed )
       count))

(comment
  (let [lines (map parse-line (ac/get-input-lines "dec-3.txt"))
        claimed (into #{} (get-multi-claimed lines))]
    (filter #(not-any? claimed (line->taken-coords %)) lines)))