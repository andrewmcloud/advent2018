(ns advent2018.day3
  (:require [clojure.java.io :as io]))

(def claim-regex #"(?<claimNumber>#\d+) @ (?<fromLeft>\d+),(?<fromRight>\d+): (?<width>\d+)x(?<height>\d+)")

(defn- build-claim-map
  [input]
  (let [matcher (re-matcher claim-regex input)]
    (when (.matches matcher)
      {:claim (.group matcher "claimNumber")
       :from-left (Integer/parseInt (.group matcher "fromLeft"))
       :from-top (Integer/parseInt (.group matcher "fromRight"))
       :width (Integer/parseInt (.group matcher "width"))
       :height (Integer/parseInt (.group matcher "height"))})))

(defn- parse-claim-file [f]
  (with-open [rdr (io/reader (io/resource f))]
    (->> (line-seq rdr)
         (mapv build-claim-map))))

(defn- cloth-size
  [claims]
  (reduce (fn [[max-width max-height] claim]
            (let [w (+ (:from-left claim) (:width claim))
                  h (+ (:from-top claim) (:height claim))]
              [(max max-width w) (max max-height h)]))
          [0 0]
          claims))

(defn- flatten-claim
  [claim cloth-size]
  (let [[x y] cloth-size
        start (+ (* y (:from-top claim))
                 (inc (:from-left claim)))]
    {:claim (:claim claim)
     :order (flatten
              (for [h (range (:height claim))
                    w (range (:width claim))]
                (+ (* x h)
                   (+ start w))))}))

(defn- draw-cut
  [fabric order]
  (reduce (fn [fabric claim]
            (if (nil? (get fabric claim))
              (assoc fabric claim (:claim order))
              (assoc fabric claim "x")))
          fabric
          (:order order)))

(defn run []
  (let [claims (parse-claim-file "day3-input.txt")
        cloth-size (cloth-size claims)
        flat-claims (map #(flatten-claim % cloth-size) claims)
        draw-cuts (reduce draw-cut {} flat-claims)
        fabric (reduce draw-cut {} flat-claims)]
    {:part1 (->> draw-cuts
                 (filter (fn [[k v]] (= "x" v)))
                 count)
     :part2 (->> (map (fn [curr]
                        (let [in-fabric-size (count (filter (fn [[k v]] (= (:claim curr) v)) fabric))
                              in-order-size (count (:order curr))]
                          (when (= in-fabric-size in-order-size)
                            (:claim curr))))
                   flat-claims)
                 (filter some?)
                 first)}))