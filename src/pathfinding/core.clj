(ns pathfinding.core)

(defn- init-table
  "Creates the inital costs table.  The table is a map where the keys are the nodes.
  The values are a map with the keys :cost :prev and :visited?"
  [graph src]
  (->
    (reduce
      (fn [m k] (assoc m k
                         {:cost Double/POSITIVE_INFINITY
                          :prev nil
                          :visited? false}))
      {}
      (keys graph))
    (assoc src {:cost 0
                :prev nil
                :visited? true})))

(defn- cost
  "Returns the cost to get to the destination.  In the case where
  a costs table is used, it uses the currently calculated cost from
  the table.  When using the graph, it gives the cost of the edge that
  is directly between the src and dst or infinity if no edge exists."
  ([costs dst]
   (get-in costs [dst :cost]))
  ([graph src dst]
    (get-in graph [src dst] Double/POSITIVE_INFINITY)))

(defn- neighbors
  [graph node]
  (keys (graph node)))

(defn- visit
  "Marks the node cur as visited and updates the costs to its neighbors.  Returns
  the updated costs table."
  [graph costs cur]
  (loop [rval costs
         [n & rest] (neighbors graph cur)]
    (if n
      (let [old (cost costs n)
            cur-cost (+ (cost costs cur) (cost graph cur n))]
        (if (< cur-cost old)
          (recur (update-in rval [n] assoc :cost cur-cost :prev cur) rest)
          (recur rval rest)))
      (assoc-in rval [cur :visited?] true))))

(defn- next-node
  "Returns the key of the lowest-cost unvisited node or nil if all nodes are visited."
  [costs]
  (reduce
    (fn [cheapest node]
      (if (and cheapest
               (< (cost costs cheapest)
                  (cost costs node)))
        cheapest
        node))
    nil
    (remove #(get-in costs [% :visited?]) (keys costs))))

(defn build-paths
  "Calculates shortest path table from src to each reachable node in the graph.

  The table keys are each possible destination.  The values are themselves a map
  with the keys: :cost, :prev, and :visited?.  The value for :prev is the prior node
  in the path, allowing the path to be traced back to the source"
  [graph src]
  (loop [costs (init-table graph src)
         current src]
    (if (and current (not= (cost costs current) Double/POSITIVE_INFINITY))
      (let [new-costs (visit graph costs current)]
        (recur new-costs (next-node new-costs)))
      costs)))

(defn shortest
  "Returns the cost and shortest path to the destination"
  [table dst]
  (let [cost (get-in table [dst :cost])]
    (loop [cur dst
           path nil]
      (if cur
        (recur
          (get-in table [cur :prev])
          (cons cur path))
        [cost path]))))

(defn all-shortest
  [table]
  (map (partial shortest table) (sort (keys table1))))

(defn shortest-cost
  [table dst]
  (get-in table [dst :cost]))

(def graph1
  {:a {:b 20 :d 80 :g 90}
   :b {:f 10}
   :c {:f 50 :h 20}
   :d {:g 20}
   :e {:b 50 :g 30}
   :f {:c 10 :d 40}
   :g {:a 20}
   :h {}})

(def table1 (build-paths graph1 :a))
