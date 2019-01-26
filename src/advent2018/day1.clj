(ns advent2018.day1
  (:require [clojure.java.io :as io]))

(defn- read-freq-file [f]
  (with-open [rdr (io/reader (io/resource f))]
    (reduce #(conj %1 (Integer/parseInt %2)) [] (line-seq rdr))))

(defn run []
  (let [freq-updates (read-freq-file "day1-input.txt")]
    {:part1 (reduce + 0 freq-updates)
     :part2 (loop [freq-updates (cycle freq-updates)
                   unique-freqs #{0}
                   current-freq 0]
              (let [current-freq (+ current-freq (first freq-updates))]
                (if (contains? unique-freqs current-freq)
                  current-freq
                  (recur (rest freq-updates)
                         (conj unique-freqs current-freq)
                         current-freq))))}))