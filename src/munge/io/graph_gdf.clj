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
            [schema.core :as s]
            [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [munge.schema :refer [Graph Nil]])
  (:import [java.io.File]
           [schema.core One Schema]))

(def Node {s/Keyword s/Any})
(def Edge (s/pred (partial satisfies? lg/Edge)))

(def +types+
  "Mapping of schema primitives to GDF type annotations."
  {s/Int "INT"
   s/Str "VARCHAR"
   s/Num "DOUBLE"
   s/Bool "BOOLEAN"})

(def required-node-fields [(s/one s/Str "name")])

(def required-edge-fields [(s/one s/Str "node1")
                           (s/one s/Str "node2")
                           (s/one s/Int "weight")])

(s/defn serialize-node :- s/Str
  [node-schema :- Schema
   get-name :- (s/=> s/Str Node)
   g :- Graph
   n :- Node]
  (let [n-props (merge (lattr/attrs g n)
                       (if (map? n) n nil)
                       {:name (get-name n)})]
    (join "," (for [field (concat required-node-fields node-schema)
                    :let [k (-> (:name field) keyword)
                          schema (:schema field)]]
                (if (= schema s/Str)
                  (format "'%s'" (n-props k)) ; single quote string values
                  (n-props k))))))

(s/defn serialize-edge :- s/Str
  [edge-schema :- Schema
   get-name :- (s/=> s/Str Node)
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
                (if (= schema s/Str)
                  (format "'%s'" (e-props k)) ; single quote string values
                  (e-props k))))))

(s/defn field-header :- s/Str
  [schema :- One]
  (format "%s %s" (:name schema) (-> (:schema schema) +types+)))

(s/defn node-header :- s/Str
  [node-schema :- [One]]
  (format "nodedef> %s" (join "," (map field-header (concat required-node-fields node-schema)))))

(s/defn edge-header :- s/Str
  [edge-schema :- [One]]
  (format "edgedef> %s" (join "," (map field-header (concat required-edge-fields edge-schema)))))

;; TODO: implement me
(s/defn read-gdf :- Graph
  [lines :- [s/Str]]
  nil)

(s/defn write-gdf :- Nil
  [ser-n ser-e node-schema edge-schema w ns es]
  (.write w (format "%s\n" (node-header node-schema)))
  (doseq [n ns]
    (.write w (format "%s\n" (ser-n n))))
  (.write w (format "%s\n" (edge-header edge-schema)))
  (doseq [e es]
    (.write w (format "%s\n" (ser-e e)))))

;; TODO: implement me
(s/defn load-gdf :- Graph
  [path :- s/Str]
  nil)

(s/defn save-gdf :- Nil
  [g :- Graph
   node-schema :- Schema
   edge-schema :- Schema
   get-name :- (s/=> s/Str Node)
   path :- s/Str]
  (let [srl-node (partial serialize-node node-schema get-name g)
        srl-edge (partial serialize-edge edge-schema get-name g)]
    (with-open [w (io/writer path)]
      (write-gdf srl-node srl-edge node-schema edge-schema w (lg/nodes g) (lg/edges g)))))
