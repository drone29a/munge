(ns munge.io.graph-gml
  "Support the GraphML file format for representing graphs."
  (:require [schema.core :as s]
            [loom.graph :as lg]
            [loom.attr :as lattr]
            [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [munge.schema :refer [Graph Nil]]))

(def Node {s/Keyword s/Any})
(def Edge (s/pred (partial satisfies? lg/Edge)))

(def header "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"  
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
    xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns
     http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n")

(s/defn node->str-with-attrs :- s/Str
  [n :- Node]
  (format "<node id=\"%s\" %s />"
          (:id n)
          (->> (dissoc n :id)
               (map (fn [[k v]]
                      (format "%s=\"%s\"" (name k) v)))
               (join " "))))

(s/defn edge->str :- s/Str
  [e :- Edge]
  (format "<edge source=\"%s\" target=\"%s\"/>"
          (-> e lg/src :id)
          (-> e lg/dest :id)))

(s/defn write-gml :- Nil
  [ser-n ser-e w ns es]
  (.write w header)
  (.write w "<graph id=\"G\" edgedefault=\"undirected\">\n")
  (doseq [n ns]
    (.write w (format "%s\n" (ser-n n))))
  (doseq [e es]
    (.write w (format "%s\n" (ser-e e))))
  (.write w "</graph>\n")
  (.write w "</graphml>"))

(s/defn save-gml :- Nil
  [g :- Graph
   path :- java.io.File]
  (let [ns (lg/nodes g)]
    (with-open [w (io/writer path)]
      (write-gml node->str-with-attrs edge->str w ns (lg/edges g)))))

(s/defn read-gml :- Nil
  []
  (throw "Implement me."))

(s/defn load-gml :- Nil
  []
  (throw "Implement me."))


