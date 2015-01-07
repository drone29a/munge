(ns munge.io.matrix-mm
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.matrix :as mx]
            [munge.schema :refer :all]
            [schema.core :as sc]
            [schema.macros :as sm])
  (:import [mikera.matrixx.impl SparseRowMatrix]))

(sm/defn read-matrix :- Matrix
  [lines :- [String]]
  (let [split-line #(str/split % #"\s+")
        parse-header (fn [[nr nc nnz]]
                       [(Double/parseDouble nr)
                        (Double/parseDouble nc)
                        (Double/parseDouble nnz)])
        ;; dec i and j, CTF matrices are 1-based
        parse-tokens (fn [[i j x]]
                       [(dec (Double/parseDouble i))
                        (dec (Double/parseDouble j))
                        (Double/parseDouble x)])
        [header-lines rest-lines] (split-with #(.startsWith % "%") lines)
        [nrows ncols nnz] (parse-header (split-line (first rest-lines)))
        m (mx/matrix (mikera.matrixx.impl.SparseRowMatrix/create nrows ncols))]
    (doseq [l (rest rest-lines)]
      (let [[i j x] (parse-tokens (split-line l))]
        (mx/mset! m i j x)))
    m))

(sm/defn write-matrix :- [String]
  [m :- Matrix]
  (let [nrows (mx/row-count m)
        ncols (mx/column-count m)
        cs (mx/columns m)
        nnz (mx/non-zero-count m)]
    ;; TODO: Can we get the element type from matrix and modify header appropriately?
    (concat ["%%MatrixMarket matrix coordinate real general\n"
             "%\n"
             (format "%s %s %s\n" nrows ncols nnz)]
            (mapcat (fn [[ci c]]
                      (for [ri (mx/non-zero-indices c)]
                        (format "%s %s %s\n" (inc ri) (inc ci) (mx/mget c ri))))
                    (map-indexed vector cs)))))

(comment (sm/defn write-matrix :- [String]
           [m :- Matrix]
           (let [nrows (mx/row-count m)
                 ncols (mx/column-count m)
                 nzs (->> m non-zeros (sort-by (comp vec reverse first))) ; sort into column-major order
                 nnz (count nzs)]
             ;; TODO: Can we get the element type from matrix and modify header appropriately?
             (concat ["%%MatrixMarket matrix coordinate real general\n"
                      "%\n"
                      (format "%s %s %s\n" nrows ncols nnz)]
                     (doall (map (fn [[i j v]]
                                   (format "%s %s %s\n" (inc i) (inc j) v))
                                 (map (partial apply conj) nzs)))))))

(sm/defn load-matrix :- Matrix
  [path :- String]
  (with-open [r (io/reader path)]
    (read-matrix (line-seq r))))

(sm/defn save-matrix
  [m :- Matrix
   path :- String]
  (with-open [w (io/writer path)]
    (doseq [l (write-matrix m)]
      (.write w l))))
