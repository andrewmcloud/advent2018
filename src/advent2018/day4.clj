(ns advent2018.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def guard-message-regex #"\[(?<date>\d+\-\d+\-\d+) (?<hour>\d+):(?<minute>\d+)\] (?<status>\S+) (?<id>\S+) ?.*")

(defn- parse-line
  [line]
  (let [matcher (re-matcher guard-message-regex line)]
    (when (.matches matcher)
      (let [date (Integer/parseInt (s/replace (.group matcher "date") #"-" ""))]
        {:date date
         :hour (Integer/parseInt (.group matcher "hour"))
         :minute (Integer/parseInt (.group matcher "minute"))
         :status (keyword (.toLowerCase (.group matcher "status")))
         :id (s/replace (.group matcher "id") #"#" "")}))))

(defn- guard-action-reducer
  [{:keys [guard start] :as acc} {:keys [status id minute]}]
  (case status
    :guard (assoc acc :guard id)
    :falls (assoc acc :start minute)
    :wakes (update acc guard conj [minute start])))

(defn- time-asleep
  [sleep-vec]
  (->> sleep-vec
       (map #(apply - %))
       (apply +)))

(defn- sleepy-guard
  [guard-sched]
  (->> (reduce (fn [coll [id sleep]]
                 (assoc coll id (time-asleep sleep)))
               {}
               (seq guard-sched))
       (apply max-key val)
       key))

(defn- sleepy-minute
  [guard-sleep]
  (->> guard-sleep
       (mapcat (fn [[end start]] (range start end)))
       frequencies
       (apply max-key val)))

(defn run []
  (with-open [rdr (io/reader (io/resource "day4-input.txt"))]
    (let [guard-sched (->> (line-seq rdr)
                           (mapv parse-line)
                           (sort-by (juxt :date :hour :minute))
                           (reduce guard-action-reducer {})
                           (#(dissoc % :guard :start "1667")))
          guard (sleepy-guard guard-sched)
          minute (key (sleepy-minute (get guard-sched guard)))]
      {:part1 (* (Integer/parseInt guard) minute)
       :part2 (->> (reduce (fn [coll [id sleep]]
                             (assoc coll id (sleepy-minute sleep)))
                           {}
                           guard-sched)
                   (apply max-key (comp second val))
                   (#(* (Integer/parseInt (key %)) (first (val %)))))})))






