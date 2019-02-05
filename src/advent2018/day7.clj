(ns advent2018.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn- parse-tuple
  [tuple]
  {:move (keyword (first tuple)) :key (keyword (last tuple))})

(defn- parse-input []
  (->> "day7-input.txt"
       io/resource
       slurp
       s/split-lines
       (map #(parse-tuple (rest (re-seq #"[A-Z]" %))))))

(defn- insert-step
  [coll step-map]
  (let [key (:key step-map)
        step (:move step-map)]
    (if (key coll)
      (update coll key conj step)
      (assoc coll key #{step}))))

(defn- insert-steps
  [steps]
  (reduce insert-step {} steps))

(defn- remove-move
  [move-map move-key pred]
  (if (empty? (remove pred (move-key move-map)))
    (dissoc move-map move-key)
    move-map))

(defn- no-key-candidate
  [move-map val-set]
  (->> val-set
       (map (fn [x]
              (when (nil? (x move-map)) x)))
       (filter some?)))

(defn no-key-candidates
  [move-map val-sets pred]
  (->> val-sets
       (map #(no-key-candidate move-map %))
       flatten
       set
       (remove pred)
       (map #(vector % %))))

(defn- next-move
  [move-map pred]
  (let [extra (no-key-candidates move-map (vals move-map) pred)
        moves (map (fn [[k v]]
                     (let [candidates (remove pred v)]
                       (when (empty? candidates) [k k])))
                   move-map)]
    (->> moves
         (filter some?)
         (reduce conj extra)
         (sort-by second)
         first)))

(defn run []
  {:part1 (loop [move-map (insert-steps (parse-input))
                 moves []]
            (if (empty? move-map)
              (s/join (map name moves))
              (let [pred (set moves)
                    next (next-move move-map pred)
                    key (first next)
                    move (last next)
                    updated-move-map (remove-move move-map key pred)]
                (recur updated-move-map (conj moves move)))))})