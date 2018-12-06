(ns advent-of-code-2018.dec-2
  (:require [medley.core :as medley]))

(defn- line-sig [line]
  (let [counts (->> (group-by identity line)
                    (map (comp count second))
                    (into #{}))]
    (clojure.set/intersection counts #{2 3})))

(defn solve-1 [input-lines]
  (let [signatures (map line-sig input-lines)
        twos (count (filter #(% 2) signatures))
        threes (count (filter #(% 3) signatures))]
    (* twos threes)))

;(solve-1 (advent-of-code-2018.core/get-input-lines "dec-2.txt"))

(defn- get-pairs [lines distance]
  (for [a lines
        b lines
        :let [char-pairs (map vector a b)
              differing-chars (->> char-pairs
                                   (filter (partial apply not=))
                                   count)
              ]
        :when (and (= differing-chars distance) (neg? (.compareTo a b)))]
    char-pairs))

(defn solve-2 [input-lines]
  (let [char-pairs (first (get-pairs input-lines 1))]
    (apply str (map first (remove (partial apply not=) char-pairs)))))