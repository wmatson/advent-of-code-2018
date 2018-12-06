(ns advent-of-code-2018.dec-5)

(def reactors
  (apply clojure.set/union
         (for [i (map char (range (int \A) (inc (int \Z))))
               :let [[lower] (clojure.string/lower-case i)]]
           #{[i lower] [lower i]})))

(defn interact? [pair]
  (reactors pair)
  #_(and left right
         (not= left right)
         (= (clojure.string/lower-case left) (clojure.string/lower-case right))))

(defn- even-step [polymer]
  (->> (partition-all 2 polymer)
       (remove reactors)
       (apply concat)))

(defn replace-step [polymer]
  (let [[f & r] (even-step polymer)
        odd-step (cons f (even-step r))]
    odd-step))

(defn reaction-seq [polymer]
  (cons polymer
        (->> (iterate replace-step polymer)
             (partition-all 2)
             (take-while (partial apply not=))
             (map second)
             (map (partial apply str)))))

(comment (reaction-seq "dabAcCaCBAcCcaDA"))

(defn solve-1 []
  (time (count (last (reaction-seq (slurp (clojure.java.io/resource "dec-5.txt")))))))

(defn fast-fully-reacted [polymer]
  (reduce (fn [reacted next]
            (let [left (peek reacted)]
              (if (interact? [left next])
                (pop reacted)
                (conj reacted next))))
          []
          polymer))

(defn fast-solve-1 []
  (count
    (fast-fully-reacted
      (slurp (clojure.java.io/resource "dec-5.txt")))))

(defn solve-2 []
  (let [input (slurp (clojure.java.io/resource "dec-5.txt"))
        results (for [unit (map char (range (int \A) (inc (int \Z))))
                      :let [[lower] (clojure.string/lower-case unit)]]
                  (->> (remove #{unit lower} input)
                       fast-fully-reacted
                       count))]
    (println results)
    (apply min results)))