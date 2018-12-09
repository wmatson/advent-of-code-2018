(ns advent-of-code-2018.dec-8)

(def sample-input [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(defn get-input []
  (->> (clojure.string/split (slurp (clojure.java.io/resource "dec-8.txt")) #"\W+")
       (map #(Integer/parseInt %))
       vec))

(defn- recursive-solve-1 [remaining-data]
  (let [[num-nodes num-metadata & leftovers] remaining-data
        [m leftover] (reduce (fn [[meta r] _]
                               (let [[m rr] (recursive-solve-1 r)]
                                 [(concat meta m) rr]))
                             [[] leftovers]
                             (range num-nodes))]
    [(concat m (take num-metadata leftover)) (drop num-metadata leftover)]))

(defn- solve-1 [input]
  (apply + (first (recursive-solve-1 input))))

(defn- recursive-solve-2 [remaining-data]
  (let [[num-nodes num-metadata & leftovers] remaining-data
        [m leftover] (reduce (fn [[meta r] _]
                               (let [[m rr] (recursive-solve-2 r)]
                                 [(conj meta m) rr]))
                             [[] leftovers]
                             (range num-nodes))]
    [(concat (take num-metadata leftover) [m]) (drop num-metadata leftover)]))

(defn- get-value [node]
  (let [meta (butlast node)
        children (last node)]
    (if (empty? children)
      (apply + meta)
      (apply + (map get-value (map #(get children (dec %) []) meta))))))

(defn- solve-2 [input]
  (let [root (first (recursive-solve-2 input))]
    (get-value root)))
