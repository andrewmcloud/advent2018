(ns advent2018.day5
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def lower-chars (mapv char (range (int \a) (+ (int \a) 26))))
(def upper-chars (mapv char (range (int \A) (+ (int \A) 26))))
(def filter-fn
  (set (concat (map (fn [l u] [l u]) lower-chars upper-chars)
               (map (fn [l u] [u l]) lower-chars upper-chars))))

(defn- remove-reacting
  [s]
  (->> s
       (partition 2)
       (remove filter-fn)
       flatten))

(defn- process-even
  [s]
  (let [filtered1 (remove-reacting s)
        filtered2 (remove-reacting (rest filtered1))]
    (str (first filtered1)
         (s/join filtered2)
         (last filtered1))))

(defn- process-odd
  [s]
  (let [filtered1 (remove-reacting s)
        filtered2 (remove-reacting (vec (str (clojure.string/join (rest filtered1))
                                             (last s))))]
    (str (first filtered1)
         (clojure.string/join filtered2))))

(defn- part1
  [input]
  (loop [s input]
    (let [intermediate (if (even? (count s))
                         (process-even s)
                         (process-odd s))]
      (if (= (count s) (count intermediate))
        (count intermediate)
        (recur (vec intermediate))))))

(defn run []
  (let [input (s/trim (slurp (io/resource "day5-input.txt")))]
    {:part1 (part1 input)
     :part2 (->> (for [x (range 26)
                       :let [f #{(get lower-chars x) (get upper-chars x)}]]
                   (part1 (remove f (vec input))))
                 (apply min))}))