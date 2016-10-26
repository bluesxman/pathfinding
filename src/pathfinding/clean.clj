(ns pathfinding.clean
  (:require [clojure.pprint :refer [pprint]]))

(def my-graph
  {:a {:b 20 :d 80 :g 90}
   :b {:f 10}
   :c {:f 50 :h 20}
   :d {:g 20}
   :e {:b 50 :g 30}
   :f {:c 10 :d 40}
   :g {:a 20}
   :h {}})

(defn neighbors
  [graph node]
  (keys (graph node)))

(defn edge-cost
  [graph from to]
  (get-in graph [from to] Double/POSITIVE_INFINITY))

(defn init-paths
  [graph from]
  (->
    (zipmap (keys graph)
            (repeat
              {:previous nil
               :cost     Double/POSITIVE_INFINITY
               :visited? false}))
    (assoc-in [from :cost] 0)))

(def my-paths (init-paths my-graph :a))

(defn path-cost
  [paths node]
  (get-in paths [node :cost]))

(defn visited?
  [[_ v]]
  (v :visited?))

(defn next-to-visit
  [paths]
  (let [unvisited (remove visited? paths)]
    (if (empty? unvisited)
      nil
      (loop [[entry & rest] unvisited
              min-node (key entry)
              min-cost (:cost (val entry))]
        (if-let [[cur-node {cur-cost :cost}] entry]
          (if (< cur-cost min-cost)
            (recur rest cur-node cur-cost)
            (recur rest min-node min-cost))
          min-node)))))

(defn visit
  [graph paths node]
  (loop [rval paths
         [neighbor & rest] (neighbors graph node)]
    (if neighbor
      (let [new-cost (+ (path-cost rval node) (edge-cost graph node neighbor))
           old-cost (path-cost rval neighbor)]
       (if (< new-cost old-cost)
         (recur (update-in rval [neighbor] assoc :cost new-cost :previous node) rest)
         (recur rval rest)))
      (assoc-in rval [node :visited?] true))))

(defn shortest-table
  [graph from]
  (loop [paths (init-paths graph from)
         current (next-to-visit paths)]
    (if (and current (not= (path-cost paths current) Double/POSITIVE_INFINITY))
      (let [updated-paths (visit graph paths current)]
        (recur updated-paths (next-to-visit updated-paths)))
      paths)))

(defn shortest-path
  [table to]
  (if (= Double/POSITIVE_INFINITY (path-cost table to))
    nil
    (loop [rval (list to)
          cur to]
     (let [{prev :previous} (table cur)]
       (if prev
         (recur (cons prev rval) prev)
         rval)))))

(def a-table (shortest-table my-graph :a))

(def a-path (partial shortest-path a-table))

(def a-cost (partial path-cost a-table))