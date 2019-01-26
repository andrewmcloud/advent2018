(ns advent2018.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn- read-string-file [f]
  (with-open [rdr (io/reader (io/resource f))]
    (reduce conj [] (line-seq rdr))))

(defn- letter-frequencies
  [barcode]
  (reduce (fn [coll [k v]]
            (conj coll v))
          #{}
          (frequencies barcode)))

(defn- process-barcodes
  [barcodes]
  (map letter-frequencies barcodes))

(defn- count-letters
  [processed-barcodes]
  (reduce (fn [result counts]
            (cond
              (and (contains? counts 3) (contains? counts 2)) (update result :both inc)
              (contains? counts 2) (update result :2 inc)
              (contains? counts 3) (update result :3 inc)
              :else result))
          {:2 0 :3 0 :both 0}
          processed-barcodes))

(defn- but-one
  [s1 s2]
  (let [seq1 (seq s1)
        seq2 (seq s2)]
    (when (= 1 (->> (map compare seq1 seq2)
                    (filter (comp not zero?))
                    (count)))
          [s1 s2])))

(defn- common-letters
  [[seq1 seq2]]
  (filter some? (map #(when (zero? (compare %1 %2)) %1) seq1 seq2)))

(defn run []
  (let [inventory-ids (read-string-file "day2-input.txt")
        count-map (count-letters (process-barcodes inventory-ids))
        but-one-list (for [a inventory-ids
                           b inventory-ids]
                       (but-one a b))]
    {:part1 (* (+ (:2 count-map) (:both count-map))
               (+ (:3 count-map) (:both count-map)))
     :part2 (->> but-one-list
                 (filter some?)
                 first
                 common-letters
                 s/join)}))