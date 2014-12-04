(ns munge.io.matrix-ctf
  (require [clojure.string :refer [split]]
           [clojure.java.io :as io]
           [clojure.core.matrix :as mx]
           [munge.core :refer :all]))

(defn read-matrix [lines]
  "Loads a matrix in coordinate text format."
  (let [split-line #(split % #"\s+")
        parse-header (fn [[nr nc nnz]]
                       [(Double/parseDouble nr)
                        (Double/parseDouble nc)
                        (Double/parseDouble nnz)])
        ;; dec i and j, CTF matrices are 1-based
        parse-tokens (fn [[i j x]]
                       [(dec (Double/parseDouble i))
                        (dec (Double/parseDouble j))
                        (Double/parseDouble x)])
        [nrows ncols nnz] (parse-header (split-line (first lines)))
        m (mx/zero-matrix nrows ncols)]
    (doseq [l (rest lines)]
      (let [[i j x] (parse-tokens (split-line l))]
        (mx/mset! m i j x)))
    m))

(defn write-matrix [m]
  "Serialize a matrix into CTF, returns seq of lines."
  (let [nrows (mx/row-count m)
        ncols (mx/column-count m)
        nzs (sort-by (comp second first) (non-zeros m)) ; sort into column-major order
        nnz (count nzs)]
    (cons (format "%s %s %s\n" nrows ncols nnz)
          (map (fn [[i j v]]
                 (format "%s %s %s\n" (inc i) (inc j) v))
               (map (partial apply conj) nzs)))))

(defn load-matrix [path]
  (with-open [r (io/reader path)]
    (read-matrix (line-seq r))))

(defn save-matrix [m path]
  (with-open [w (io/writer path)]
    (doseq [l (write-matrix m)]
      (.write w l))))
