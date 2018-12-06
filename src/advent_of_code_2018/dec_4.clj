(ns advent-of-code-2018.dec-4
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter])
  (:require [advent-of-code-2018.core :as ac]
            [medley.core :as medley]))

(def datetime-format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))

(defn parse-line [line]
  (let [[_ datetime-string guard-id action] (re-find #"\[(\d{4}-\d\d-\d\d \d\d:\d\d)\] (?:Guard #(\d+) )?(.+)" line)]
    (medley/assoc-some
      {:datetime (LocalDateTime/parse datetime-string datetime-format)
       :action   action}
      :guard-id guard-id)))

(defn lines->shifts [lines]
  (->> lines
       (sort-by :datetime)
       (reduce (fn [[current-shift & complete-shifts :as shifts] {:keys [guard-id] :as next-entry}]
                 (if (some? guard-id)
                   (cons [next-entry] shifts)
                   (cons (conj current-shift next-entry) complete-shifts)))
               [])))

(defn minutes-between [^LocalDateTime start ^LocalDateTime end]
  (take-while #(.isBefore % end)
              (iterate #(.plusMinutes % 1) start)))

(defn shift->guard-sleep-seconds [shift]
  (let [guard-id (:guard-id (first shift))
        sleep-minutes (->> (partition-all 2 1 shift)
                           (filter (comp #{"falls asleep"} :action first))
                           (map #(map :datetime %))
                           (mapcat (partial apply minutes-between))
                           #_(into #{}))]
    {:guard-id guard-id
     :sleep-minutes sleep-minutes
     :shift shift}))

(defn sleepiest-1 [guard-sleep-seconds]
  (->> guard-sleep-seconds
       (group-by :guard-id)
       (apply max-key (comp count #(mapcat :sleep-minutes %) second))))

(defn get-guard-sleep-shifts []
  (->> (ac/get-input-lines "dec-4.txt")
       (map parse-line)
       lines->shifts
       (map shift->guard-sleep-seconds)))

(defn- highest-frequency [freqs]
  (if (empty? freqs)
    nil
    (apply max-key second freqs)))

(defn sleepiest-minute [shifts]
  (->> shifts
       (mapcat :sleep-minutes)
       (map #(.getMinute %))
       frequencies
       highest-frequency))

(defn solve-1 []
  (let [[guard-id shifts]
        (->> (get-guard-sleep-shifts)
             sleepiest-1)]
    (* (Integer/parseInt guard-id) (first (sleepiest-minute shifts)))))

(defn solve-2 []
  (let [[guard-id [minute]]
        (->> (get-guard-sleep-shifts)
             (group-by :guard-id)
             (medley/map-vals sleepiest-minute)
             (remove (comp nil? second))
             (apply max-key (comp second second)))]
    (* (Integer/parseInt guard-id) minute)))


(comment
  (medley/map-vals (partial map #(.getMinute %)))
  (medley/map-vals frequencies)
  (remove (comp empty? second))
  (apply max-key (comp second second second))
  (take 2))

