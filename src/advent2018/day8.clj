(ns advent2018.day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn split-at-space [s]
  (-> s
      str/trim
      (str/split #" ")))

(defn read-input []
  (->> "day8-input.txt"
       io/resource
       slurp
       split-at-space
       (map #(Integer/parseInt %))))

(defn sum-metadata  [children entries]
  (reduce + (apply conj (mapv :metadata children) entries)))

(declare traverse-forest)

(defn traverse-tree
  [[child-ct metadata & rest :as input]]
  (if (zero? child-ct)
    {:metadata (reduce + (take metadata rest))
     :remaining (drop metadata rest)}
    (traverse-forest input)))

(defn traverse-forest
  [[child-ct metadata & rest]]
  (loop [children []
         remaining rest
         traversed 0]
    (if (= traversed child-ct)
      {:metadata (sum-metadata children (take metadata remaining))
       :remaining (drop metadata remaining)}
      (let [child (traverse-tree remaining)]
        (recur
          (conj children child)
          (:remaining child)
          (inc traversed))))))

(defn run []
  {:part1 (:metadata (traverse-tree (read-input)))})