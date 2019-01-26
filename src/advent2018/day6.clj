(ns advent2018.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-input []
  (let [int-re-xf (comp
                   (map (partial re-seq #"\d+"))
                   (map (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)])))]
    (->> "day6-input.txt"
         io/resource
         slurp
         s/split-lines
         (into [] int-re-xf))))

(defn manhattan
  [[x y] [a b]]
  (+ (Math/abs ^int (- x a))
     (Math/abs ^int (- y b))))

(defn bounded?
  [coord-map max-x max-y]
  (let [[x y] (:point coord-map)]
    (if (and (< 0 x max-x) (< 0 y max-y))
      (assoc coord-map :bounded? true)
      (assoc coord-map :bounded? false))))

(defn point->coords
  [point coords max-x max-y]
  (let [closest (->> (map-indexed (fn [idx coord] {:idx idx :dist (manhattan point coord) :point point}) coords)
                     (sort-by :dist)
                     (partition-by :dist)
                     first)]
    (if (= 1 (count closest))
      (bounded? (first closest) max-x max-y)
      nil)))

(defn region
  [point coords]
  (let [total-dist (reduce + (map #(manhattan point %) coords))]
    (when (< total-dist 10000)
      true)))

(defn unbounded-coords
  [distance-maps]
  (let [unbounded (filter #(false? (:bounded? %)) distance-maps)]
    (reduce (fn [coll m]
              (conj coll (:idx m)))
            #{}
            unbounded)))

(defn run []
  (let [coords (parse-input)
        max-x (apply max (map first coords))
        max-y (apply max (map second coords))
        grid-points (for [x (range 0 (inc max-x))
                          y (range 0 (inc max-y))]
                      [x y])
        distance-maps (filter some? (map #(point->coords % coords max-x max-y) grid-points))
        unbounded (unbounded-coords distance-maps)]
    {:part1 (->> distance-maps
                 (remove (comp unbounded :idx))
                 (group-by :idx)
                 vals
                 (map count)
                 (sort >)
                 first)
     :part2 (->> grid-points
                 (map #(region % coords))
                 (filter true?)
                 count)}))

