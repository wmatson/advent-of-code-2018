(ns advent-of-code-2018.core)

(defn get-input-lines [filename]
  (clojure.string/split (slurp (clojure.java.io/resource filename))
                        #"[\r\n]+"))
