(ns munge.matrix
  (:require [clojure.core.matrix :as mx]
            [clojure.core.typed :as t]
            [schema.core :as s]
            [munge.schema :refer [Mat Vec BinVec ProbVec]]
            [munge.type :as mt])
  (:import [mikera.matrixx.impl SparseRowMatrix SparseColumnMatrix]
           [mikera.vectorz.impl SparseIndexedVector SparseHashedVector ASparseVector]
           [mikera.vectorz AVector]
           [mikera.vectorz.util DoubleArrays]))

(set! *warn-on-reflection* true)
;; TODO: Much of this code are workarounds for the core.matrix and Vectorz.
;;       Ideally this or related code will make it back to core.matrix.
;;       The biggest issue is the creation of sparse matrices without
;;       allocating the memory required for the dense representation.

(s/defn sparse-row-matrix :- Mat
  [sparse-vecs :- [Vec]]
  (SparseRowMatrix/create ^"[Lmikera.vectorz.AVector;" (into-array mikera.vectorz.AVector sparse-vecs)))

(s/defn sparse-column-matrix :- Mat
  [sparse-vecs :- [Vec]]
  (SparseColumnMatrix/create ^"[Lmikera.vectorz.AVector;" (into-array mikera.vectorz.AVector sparse-vecs)))

(s/defn create-sparse-indexed-vector :- Vec
  [length :- s/Int]
  (SparseIndexedVector/createLength length))

(s/defn sparse-hashed-vector :- Vec
  ([length :- s/Int]
     (SparseHashedVector/createLength length))
  ([length :- s/Int
    vals :- {s/Int s/Num}]
     (let [v (sparse-hashed-vector length)]
       (doseq [[idx val] vals]
         (.unsafeSet ^SparseHashedVector v idx val))
       v)))

(s/defn sparse-indexed-vector :- Vec
  ([v :- [s/Num]]
   (let [data (double-array v)
         n (count data)
         nz-inds (DoubleArrays/nonZeroIndices data 0 n)
         nnz (count nz-inds)
         nz-data (double-array nnz)]
     (dotimes [i nnz]
       (aset nz-data i (aget data (aget nz-inds i))))
     (SparseIndexedVector/wrap n nz-inds nz-data)))
  ([length :- s/Int
    vals :- {s/Int s/Num}]
   (SparseIndexedVector/create ^SparseHashedVector (sparse-hashed-vector length vals)))
  ([length :- s/Int
    indices :- ints
    vals :- doubles]
   (SparseIndexedVector/wrap ^int length indices vals)))

;; TODO: remove support for [s/Num] rows, treat all as maps.
(s/defn sparse-matrix :- Mat
  "Creates a sparse matrix from a collection of rows. 

  Each row may be a vector or a map."
  [nrows :- s/Int
   ncols :- s/Int
   rows :- [(s/either [s/Num] {s/Int s/Num} Vec)]]
  (sparse-row-matrix (map (fn [r] (cond
                                    (isa? (type r) mikera.vectorz.impl.ASparseVector) r
                                    (vector? r) (sparse-indexed-vector r)
                                    :else (sparse-indexed-vector ncols r))) rows)))

;; TODO: push smarter impl upstream to vectorz
;;       there should be a non-zero-vals if we want to do comps in Clojure;
;;       other option is to expose abs via matrix-api so we don't have to test
;;       if vector is derived from AVector
(s/defn l1-norm :- s/Num
  "Compute L1 norm."
  [v :- Vec]
  (if (instance? AVector v)
    (.elementSum ^AVector (.absCopy ^AVector v))
    (throw (Exception. "Unsupported vector type!"))))

(s/defn l2-norm :- s/Num
  "Compute L2 norm."
  [v :- Vec]
  (if (instance? AVector v)
    (Math/sqrt (.elementSum ^AVector (.squareCopy ^AVector v)))
    (throw (Exception. "Unsupported vector type!"))))

(s/defn proportional :- Vec
  "Normalize vector by L1-norm."
  [v :- Vec]
  (let [norm (l1-norm v)]
    (if (zero? norm)
      v
      (do (mx/div! v norm)
          v))))

;; TODO: this calls proportional, why??
(s/defn round-to-zero! :- (s/either Mat Vec)
  "Returns a new matrix (need to recompute sparse index) with 
  values below threshold set to 0. 

  Requires vectors to be sparse indexed implementations."
  [threshold :- s/Num
   m :- (s/either Mat Vec)]
  ;; TODO: best speed assumes row matrix, fixable through current core.matrix API?
  (if (mx/vec? m)
    (.roundToZero ^SparseIndexedVector m threshold)
    (let [vs (if (mx/vec? m) [m] (mx/rows m))]
      (SparseRowMatrix/create ^java.util.List (map (fn [v] (when (instance? SparseIndexedVector v)
                                                             (-> (.roundToZero ^SparseIndexedVector v threshold)
                                                                 (proportional))))
                                                   vs)))))

;; Use this until something better is included in core.matrix
;; TODO: consider optimizations that do't require individual calls to mget
;; TODO: consider optimizations when matrix is column-major
(s/defn non-zeros :- {[s/Int] s/Num}
  "Gets the non-zero indices of an array mapped to the values."
  ([m :- Mat]
     (into {} (for [row-idx (range (mx/row-count m))
                    col-idx (mx/non-zero-indices (mx/get-row m row-idx))]
                [[row-idx col-idx] (mx/mget m row-idx col-idx)]))))

;; TODO: don't use mget, clone the internal array if it's a sparseindexedvector
(s/defn vec-non-zeros :- {[s/Int] s/Num}
  "Gets the non-zero indices of an array mapped to the values.

  Warning: indices may not be in sequential order."
  ([v :- Vec] (->> (mx/non-zero-indices v)
                   (map (fn [idx] [[idx] (mx/mget v idx)]))
                   (into {}))))

(s/defn binary-vector :- BinVec
  "Create a binary vector with ones corresponding to given indices."
  [size :- s/Int
   selected-idxs :- [s/Int]]
  (let [v (mx/zero-vector size)]
    (doseq [idx selected-idxs]
      (mx/mset! v idx 1))
    v))

(s/defn b-or :- BinVec
  "Binary OR operation for BinVecs."
  [& vecs :- [BinVec]]
  (let [max-size (apply max (map mx/row-count vecs))]
    (->> (mapcat mx/non-zero-indices vecs)
         (reduce conj #{})
         seq
         (binary-vector max-size))))

(s/defn select-rows :- [Vec]
  [m :- Mat
   idxs :- [s/Int]]
  (map (partial mx/get-row m) idxs))

(s/defn selected-rows :- Mat
  "Return a matrix with unselected rows to zero."
  [m :- Mat
   row-indicator :- BinVec]

  ;; TODO: Curious when/if this is much/any slower
  (comment
    (mx/inner-product (mx/sparse (mx/diagonal-matrix row-indicator)) m))

  (let [[nrows ncols] (mx/shape m)
        new-m (SparseRowMatrix/create nrows ncols)]
    (doseq [row-idx (mx/non-zero-indices row-indicator)]
      (mx/set-row! new-m row-idx (mx/get-row m row-idx)))
    new-m))

(s/defn normalize-log-prob :- ProbVec
  "Normalize log probabilities and return as plain probabilities.
  Probabilities < 1e-10 are dropped to zero."
  [log-probs :- Vec]
  (let [epsilon 1e-10
        threshold (- (Math/log epsilon) (Math/log (mx/row-count log-probs)))
        max-prob (mx/emax log-probs)]
    (proportional (mx/emap (comp #(if (> threshold %) 0 (Math/exp %))
                                 #(- % max-prob))
                           log-probs))))

(t/defn normalize-min-max
  [v :- mt/Vec] :- mt/Vec
  (let [min-val (mx/emin v)
        max-val (mx/emax v)]
    (mx/emap #(/ (- % min-val)
                 (- max-val min-val))
             v)))

(comment (s/defn set-row-non-zeros! :- Mat
           [m :- Mat
            row-idx :- s/Int
            row :- Vec]
           (doseq [col-idx (mx/sparse [0 1 1 0 2])]
             (mx/mset! m row-idx col-idx (mx/mget row col-idx)))
           m))

