(ns munge.schema
  (:require [schema.core :as s]
            [clojure.core.matrix :as mx]
            [loom.graph :as lg]))

(def Vec
  "Any vector"
  (s/pred mx/vec? 'vec?))

(def Mat
  "Any matrix"
  (s/pred mx/matrix? 'matrix?))
(def Matrix Mat)

(defn prob-vec?
  [x]
  (and (mx/vec? x)
       (> 0.001
          (let [^double sum-x (mx/esum x)]
            (Math/abs (- 1 sum-x))))))
(def ProbVec
  "Probabilty vector"
  (s/pred prob-vec? 'prob-vec?))

(defn bin-vec?
  [x]
  (and (mx/vec? x)
       (every? #(or (== 1 %) (== 0 %)) (mx/eseq x))))
(def BinVec
  "Binary vector"
  (s/pred bin-vec? 'bin-vec?))

(defn bin-mat?
  [x]
  (and (mx/matrix? x)
       (every? #(or (== 1 %) (== 0 %)) (mx/eseq x))))
(def BinMat
  "Binary matrix"
  (s/pred bin-mat? 'bin-mat?))

(def Graph (s/protocol lg/Graph))
(def WeightedGraph (s/both (s/protocol lg/WeightedGraph) Graph))

