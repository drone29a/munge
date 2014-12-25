(ns munge.io.graph-gdf
  "Loads and saves Loom graphs from and to files in the GUESS GDF format
  (http://guess.wikispot.org/The_GUESS_.gdf_format).

  Schemas are used to encode values (e.g, surround strings with single quotes)
  and add type annotations to the node and edge headers.

  The GDF format defines a few required node and edge attributes. Nodes must have
  a name attribute and edges must have node1 and node2 attributes. Additionally, this library
  always includes a weight attribute for edges which defaults to a value of 1.

  If nodes have a name attribute or edges have node1, node2, or weight attributes, they
  will be replaced with computed values in the output."
  (:require [loom.graph :as lg]
            [loom.attr :as lattr]
            [schema.core :as sc]
            [schema.macros :as sm]
            [clojure.java.io :as io]
            [clojure.string :refer [join]])
  (:import [java.io.File]
           [schema.core One]))

(def Graph (sc/pred (partial satisfies? lg/Graph)))
(def Node {sc/Keyword sc/Any})
(def Edge (sc/pred (partial satisfies? lg/Edge)))
(def Nil (sc/pred nil?))


(def +types+
  "Mapping of schema primitives to GDF type annotations."
  {sc/Int "INT"
   sc/Str "VARCHAR"
   sc/Num "DOUBLE"
   sc/Bool "BOOLEAN"})

(def required-node-fields [(sc/one sc/Str "name")])

(def required-edge-fields [(sc/one sc/Str "node1")
                           (sc/one sc/Str "node2")
                           (sc/one sc/Int "weight")])

(sm/defn serialize-node :- sc/Str
  [node-schema :- sc/Schema
   get-name :- (sc/=> sc/Str Node)
   g :- Graph
   n :- Node]
  (let [n-props (merge (lattr/attrs g n)
                       (if (map? n) n nil)
                       {:name (get-name n)})]
    (join "," (for [field (concat required-node-fields node-schema)
                    :let [k (-> (:name field) keyword)
                          schema (:schema field)]]
                (if (= schema sc/Str)
                  (format "'%s'" (n-props k)) ; single quote string values
                  (n-props k))))))

(sm/defn serialize-edge :- sc/Str
  [edge-schema :- sc/Schema
   get-name :- (sc/=> sc/Str Node)
   g :- Graph
   e :- Edge]
  (let [e-props (merge (lattr/attrs g e)
                       {:weight (or (lg/weight g e) 1)
                        :node1 (-> (lg/src e) get-name)
                        :node2 (-> (lg/dest e) get-name)})]
    (join "," (for [field (concat required-edge-fields
                                  edge-schema)
                    :let [k (-> (:name field) keyword)
                          schema (:schema field)]]
                (if (= schema sc/Str)
                  (format "'%s'" (e-props k)) ; single quote string values
                  (e-props k))))))

(sm/defn field-header :- sc/Str
  [schema :- One]
  (format "%s %s" (:name schema) (-> (:schema schema) +types+)))

(sm/defn node-header :- sc/Str
  [node-schema :- [One]]
  (format "nodedef> %s" (join "," (map field-header (concat required-node-fields node-schema)))))

(sm/defn edge-header :- sc/Str
  [edge-schema :- [One]]
  (format "edgedef> %s" (join "," (map field-header (concat required-edge-fields edge-schema)))))

;; TODO: implement me
(sm/defn read-gdf :- Graph
  [lines :- [sc/Str]]
  nil)

(sm/defn write-gdf :- Nil
  [ser-n ser-e node-schema edge-schema w ns es]
  (.write w (format "%s\n" (node-header node-schema)))
  (doseq [n ns]
    (.write w (format "%s\n" (ser-n n))))
  (.write w (format "%s\n" (edge-header edge-schema)))
  (doseq [e es]
    (.write w (format "%s\n" (ser-e e)))))

;; TODO: implement me
(sm/defn load-gdf :- Graph
  [path :- sc/Str]
  nil)

(sm/defn save-gdf :- Nil
  [g :- Graph
   node-schema :- sc/Schema
   edge-schema :- sc/Schema
   get-name :- (sc/=> sc/Str Node)
   path :- sc/Str]
  (let [srl-node (partial serialize-node node-schema get-name g)
        srl-edge (partial serialize-edge edge-schema get-name g)]
    (with-open [w (io/writer path)]
      (write-gdf srl-node srl-edge node-schema edge-schema w (lg/nodes g) (lg/edges g)))))
