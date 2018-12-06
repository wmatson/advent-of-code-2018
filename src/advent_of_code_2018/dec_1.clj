(ns advent-of-code-2018.dec-1)

(defn get-input []
  (slurp (clojure.java.io/resource "dec-1.txt")))

(defn parse-input [input]
  (->> (clojure.string/split input #"[\r\n]+")
       (map #(Integer/parseInt %))))

(defn solve-1 [nums]
  (apply + nums))

(defn solve-2 [nums]
  (loop [[f & r] (reductions + 0 (cycle nums))
         visited? #{}]
    (if (visited? f)
      f
      (recur r (conj visited? f)))))
