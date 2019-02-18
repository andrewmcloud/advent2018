(ns advent2018.day9
  (:require [clojure.java.io :as io]))

(defn init
  [new]
  (let [k (keyword (str new))]
    {k {:prev k :next k :val k}}))

(defmulti insert-link
  (fn ([curr m new]
       (if (= (count (keys m)) 1)
         :1
         :many))))

(defmethod insert-link :1
  [{:keys [prev next val]} m new]
  (let [new (keyword (str new))]
    (-> m
        (assoc val {:next new :prev new :val val})
        (assoc new {:next next :prev prev :val new}))))

(defmethod insert-link :many
  [{:keys [prev next val]} m new]
  (let [new (keyword (str new))
        next-node (assoc (next m) :prev new)]
    (-> m
        (assoc val {:prev prev :next new :val val}) ;;current node
        (assoc new {:prev val :next next :val new}) ;;new node
        (assoc next next-node))))

(defn delete-link
  [{:keys [prev next val]} m]
  (let [next-node (assoc (next m) :prev prev)
        prev-node (assoc (prev m) :next next)]
    (-> m
        (assoc prev prev-node)
        (assoc next next-node)
        (dissoc val))))

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
  (let [position ((:next (curr ll)) ll)]
    (-> game
        (assoc :ll (insert-link position ll marble))
        (update :player #(mod (inc %) players))
        (update :marble inc)
        (assoc :curr (keyword (str marble))))))

(defn remove-marble
  [{:keys [marble player curr ll players] :as game}]
  (let [remove-node (nth-link -7 ll (curr ll))
        node-value (Integer/parseInt (name (:val remove-node)))]
    (println remove-node)
    (-> game
        (assoc :ll (delete-link remove-node ll))
        (update :player #(mod (inc %) players))
        (update :marble inc)
        (assoc :curr (:next remove-node))
        (assoc-in [:scores player] (+ marble node-value)))))

(defn parse-input
  []
  (let [in (->> "day9-input.txt"
                io/resource
                slurp
                (re-seq #"\d+"))]
    {:players (first in) :points (last in)}))


(def game
  (loop [gm {:marble 1
             :player 1
             :curr :0
             :scores {}
             :ll (init 0)
             :players 9}
         count 0]
    (if (> count 21)
      gm
      (recur (add-marble gm) (inc count)))))