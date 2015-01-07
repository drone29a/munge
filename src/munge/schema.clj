(ns munge.schema
  (:require [schema.core :as s]
            [clojure.core.matrix :as mx]
            [loom.graph :as lg])
  (:import [mikera.vectorz.impl SparseIndexedVector SparseHashedVector]
           [mikera.matrixx.impl SparseRowMatrix SparseColumnMatrix]))

;; TODO: core.matrix/vec? should return true, but returns false
(def Vec
  "Any vector"
  (s/either (s/pred mx/vec? 'vec?)
            SparseIndexedVector
            SparseHashedVector))

;; TODO: core.matrix/matrix? should return true, but returns false
(def Mat
  "Any matrix"
  (s/either (s/pred mx/matrix? 'matrix?)
            SparseRowMatrix
            SparseColumnMatrix))
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

