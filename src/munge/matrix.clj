(ns munge.matrix
  (:require [clojure.core.matrix :as mx]
            [schema.core :as s]
            [munge.schema :refer [Mat Vec BinVec ProbVec]])
  (:import [mikera.matrixx.impl SparseRowMatrix SparseColumnMatrix]
           [mikera.vectorz.impl SparseIndexedVector SparseHashedVector]
           [mikera.vectorz.util DoubleArrays]))

;; TODO: Much of this code are workarounds for the core.matrix and Vectorz.
;;       Ideally this or related code will make it back to core.matrix.
;;       The biggest issue is the creation of sparse matrices without
;;       allocating the memory required for the dense representation.

(s/defn create-sparse-row-matrix :- Mat
  [sparse-vecs :- [Vec]]
  (SparseRowMatrix/create ^"[Lmikera.vectorz.AVector;" (into-array mikera.vectorz.AVector sparse-vecs)))

(s/defn create-sparse-column-matrix :- Mat
  [sparse-vecs :- [Vec]]
  (SparseColumnMatrix/create ^"[Lmikera.vectorz.AVector;" (into-array mikera.vectorz.AVector sparse-vecs)))

(s/defn create-sparse-indexed-vector :- Vec
  [length :- s/Int]
  (SparseIndexedVector/createLength length))

(s/defn create-sparse-hashed-vector :- Vec
  ([length :- s/Int]
     (SparseHashedVector/createLength length))
  ([length :- s/Int
    vals :- {s/Int s/Num}]
     (let [v (create-sparse-hashed-vector length)]
       (doseq [[idx val] vals]
         (.unsafeSet v idx val))
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
     (SparseIndexedVector/create (create-sparse-hashed-vector length vals))))

;; TODO: remove support for [s/Num] rows, treat all as maps.
(s/defn sparse-matrix :- Mat
  "Creates a sparse matrix from a collection of rows. 

  Each row may be a vector or a map."
  [nrows :- s/Int
   ncols :- s/Int
   rows :- [(s/either [s/Num] {s/Int s/Num})]]
  (create-sparse-row-matrix (map (fn [r] (if (vector? r)
                                           (sparse-indexed-vector r)
                                           (sparse-indexed-vector ncols r))) rows)))

(s/defn proportional :- Vec
  "Normalize vector by L1-norm."
  [v :- Vec]
  (let [^ints nz-idxs (mx/non-zero-indices v)
        ;; TODO: non-zero-indices isn't guaranteed to return int[],
        ;;       but it does for the sparse data types and we want speeeeed
        num-idxs (alength nz-idxs)
        l1-norm (loop [idx (int 0)
                       sum (double 0.0)]
                  (if (> num-idxs idx)
                    (recur (inc idx)
                           (+ sum (Math/abs (double (mx/mget v (aget nz-idxs idx))))))
                    sum))]
    (if (zero? l1-norm)
      v
      (do (mx/div! v l1-norm)
          v))))

;; TODO: this calls proportional, why??
(s/defn round-to-zero! :- Mat
  "Returns a new matrix (need to recompute sparse index) with 
  values below threshold set to 0."
  [threshold :- s/Num
   m :- (s/either Mat Vec)]
  ;; TODO: best speed assumes row matrix, fixable through current core.matrix API?
  (let [vs (if (mx/vec? m) [m] (mx/rows m))]
    (SparseRowMatrix/create ^java.util.List (map (fn [v] (when (instance? SparseIndexedVector v)
                                                           (-> (.roundToZero ^SparseIndexedVector v threshold)
                                                               (proportional))))
                                                 vs))))

;; Use this until something better is included in core.matrix
(s/defn non-zeros :- {[s/Int] s/Num}
  "Gets the non-zero indices of an array mapped to the values."
  ([m :- Mat]
     (into {} (for [row-idx (range (mx/row-count m))
                    col-idx (mx/non-zero-indices (mx/get-row m row-idx))]
                [[row-idx col-idx] (mx/mget m row-idx col-idx)]))))

(s/defn vec-non-zeros :- {[s/Int] s/Num}
  "Gets the non-zero indices of an array mapped to the values."
  ([m] (into {} 
             (filter (comp not zero? second) 
                     (map vector (mx/index-seq m) (mx/eseq m))))))

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

(comment (s/defn set-row-non-zeros! :- Mat
           [m :- Mat
            row-idx :- s/Int
            row :- Vec]
           (doseq [col-idx (mx/sparse [0 1 1 0 2])]
             (mx/mset! m row-idx col-idx (mx/mget row col-idx)))
           m))

