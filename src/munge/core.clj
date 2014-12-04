(ns munge.core
  (:require [schema.core :as sc]
            [schema.macros :as sm]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix :as mx]
            [loom.graph :as lg]))
(def Vec
  "Any vector"
  (sc/pred mx/vec? 'vec?))

(def Mat
  "Any matrix"
  (sc/pred mx/matrix? 'matrix?))
(def Matrix Mat)

(defn prob-vec?
  [x]
  (and (mx/vec? x)
       (> 0.001
          (let [^double sum-x (mx/esum x)]
            (Math/abs (- 1 sum-x))))))
(def ProbVec
  "Probabilty vector"
  (sc/pred prob-vec? 'prob-vec?))

(defn bin-vec?
  [x]
  (and (mx/vec? x)
       (every? #(or (== 1 %) (== 0 %)) (mx/eseq x))))
(def BinVec
  "Binary vector"
  (sc/pred bin-vec? 'bin-vec?))

(defn bin-mat?
  [x]
  (and (mx/matrix? x)
       (every? #(or (== 1 %) (== 0 %)) (mx/eseq x))))
(def BinMat
  "Binary matrix"
  (sc/pred bin-mat? 'bin-mat?))


(def Graph (sc/protocol lg/Graph))
(def WeightedGraph (sc/both (sc/protocol lg/WeightedGraph) Graph))


(defn comb [& fns]
  (fn [& args]
    (vec (map (fn [f arg] (f arg)) fns args))))

(sm/defn update-for-keys :- {sc/Any sc/Any}
  [m :- {sc/Any sc/Any}
   f :- (sc/=> sc/Any sc/Any)
   ks :- [sc/Any]
   init-v :- [sc/Any]]
  (reduce (fn [acc-m k] (assoc acc-m k (f (get acc-m k init-v)))) m ks))

;; Use this until something better is included in core.matrix
(sm/defn non-zeros :- {[sc/Int] sc/Num}
  "Gets the non-zero indices of an array mapped to the values."
  ([m :- Mat]
     (into {} (for [row-idx (range (mx/row-count m))
                    col-idx (mx/non-zero-indices (mx/get-row m row-idx))]
                [[row-idx col-idx] (mx/mget m row-idx col-idx)]))))

(sm/defn vec-non-zeros :- {[sc/Int] sc/Num}
  "Gets the non-zero indices of an array mapped to the values."
  ([m] (into {} 
             (filter (comp not zero? second) 
                     (map vector (mx/index-seq m) (mx/eseq m))))))

(comment (sm/defn set-row-non-zeros! :- Mat
           [m :- Mat
            row-idx :- sc/Int
            row :- Vec]
           (doseq [col-idx (mx/sparse [0 1 1 0 2])]
             (mx/mset! m row-idx col-idx (mx/mget row col-idx)))
           m))
