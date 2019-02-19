(ns advent2018.day9
  (:require [clojure.java.io :as io]))

(defn keywordize-int
  [i]
  (keyword (str i)))

(defn init
  [new]
  (let [k (keywordize-int new)]
    (transient {k {:prev k :next k :val k}})))

(defmulti insert-link
  (fn [curr m new]
    (if (= 1 (count m)) :single :multi)))

(defmethod insert-link :single
  [{:keys [prev next val]} m new]
  (let [new (keywordize-int new)]
    (-> m
      (assoc! val {:next new :prev new :val val})
      (assoc! new {:next next :prev prev :val new}))))

(defmethod insert-link :multi
  [{:keys [prev next val]} m new]
  (let [new (keywordize-int new)
        next-node (assoc (next m) :prev new)]
    (-> m
        (assoc! val {:prev prev :next new :val val})
        (assoc! new {:prev val :next next :val new})
        (assoc! next next-node))))

(defn delete-link
  [{:keys [prev next val]} m]
  (let [next-node (assoc (next m) :prev prev)
        prev-node (assoc (prev m) :next next)]
    (-> m
        (assoc! prev prev-node)
        (assoc! next next-node)
        (dissoc! val))))

(defn nth-link
  [n m curr]
  (let [f (if (pos? n) :next :prev)
        n (inc (Math/abs ^int n))]
    (->> curr
         (iterate (fn [node] ((f node) m)))
         (take n)
         last)))

(defn add-marble
  [{:keys [marble player curr ll players] :as game}]
  (let [insert-position ((:next (curr ll)) ll)]
    (-> game
        (assoc :ll (insert-link insert-position ll marble))
        (update :player #(mod (inc %) players))
        (update :marble inc)
        (assoc :curr (keywordize-int marble)))))

(defn remove-marble
  [{:keys [marble player curr ll players] :as game}]
  (let [remove-node (nth-link -7 ll (curr ll))
        node-value (Integer/parseInt (name (:val remove-node)))
        score (+ marble node-value)]
    (-> game
        (assoc :ll (delete-link remove-node ll))
        (update :player #(mod (inc %) players))
        (update :marble inc)
        (assoc :curr (:next remove-node))
        (update-in [:scores player] (fnil #(+ % score) 0)))))

(defn parse-input []
  (let [in (->> "day9-input.txt"
                io/resource
                slurp
                (re-seq #"\d+"))]
    {:players (Integer/parseInt (first in))
     :points (Integer/parseInt (last in))}))

(defn play-game
  [players points]
  (loop [game {:marble 1
               :player 1
               :curr :0
               :scores {}
               :ll (init 0)
               :players players}]
    (if (= (:marble game) points)
      (apply max (vals (:scores game)))
      (if (zero? (mod (:marble game) 23))
        (recur (remove-marble game))
        (recur (add-marble game))))))

(defn run []
  (let [{:keys [players points]} (parse-input)]
    {:part1 (play-game players points)
     :part2 (play-game players (* 100 points))}))