(ns munge.io.snap
  "Functions for loading SNAP datasets."
  (:require [clojure.string :as str]
            [clojure.data.csv :as csv]
            [commune.matrix :as mat]
            [clojure.java.io :as io]
            [munge.io])
  (:import [cern.colt.matrix DoubleMatrix2D]))

(defn read-edges [lines]
  (map (comp sort
             (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)]) 
             #(str/split % #"\s+") 
             str/trim) 
       lines))

(defn load-edges [edges-path]
  (with-open [r (io/reader edges-path)]
    (doall (read-edges (line-seq r)))))

(defn mat-from-edges [es]
  (let [dim (inc (apply max (flatten es)))
        m (mat/make-sparse-matrix dim dim)]
    (doseq [[x y] es]
      (.setQuick m x y 1))
    m))

(defn convert-edges 
  "Convert edge file to adj matrix, add a vertex to represent the ego node."
  [edges-path ctf-path]
  (with-open [r (io/reader edges-path)]
    (let [edges (-> r line-seq read-edges)
          ego-vert (inc (apply max (flatten edges)))
          verts (set (flatten edges))
          ego-edges (map vector verts (repeat ego-vert))
          m (mat-from-edges (concat edges ego-edges))]
      (commune.io/save-matrix m ctf-path))))

(defn read-feats [lines]
  (map (comp (partial map #(Integer/parseInt %)) rest #(str/split % #"\s+") str/trim)
       lines))

(defn read-ego-feats [line]
  ((comp (partial map #(Integer/parseInt %))
         #(str/split % #"\s+")) line))

(defn load-ego-feats [ego-path]
  (with-open [r (io/reader ego-path)]
    (doall (read-ego-feats (first (line-seq r))))))

(defn mat-from-feats [fs]
  (let [nrows (count fs)
        ncols (count (first fs))
        m (mat/make-sparse-matrix nrows ncols)]
    (doseq [[i f-row] (map-indexed vector fs)]
      (doseq [[j val] (map-indexed vector f-row)]
        (when (not (zero? val))
          (.setQuick m i j val))))
    m))

(defn convert-feats [feats-path ego-feats-path ctf-path]
  (with-open [r (io/reader feats-path)]
    (let [feats (-> r line-seq read-feats)
          ego-feats (load-ego-feats ego-feats-path)
          m (mat-from-feats (conj feats ego-feats))]
      (commune.io/save-matrix m ctf-path))))

(defn read-circles [lines]
  (map (comp (partial map #(Integer/parseInt %))
             rest
             #(str/split % #"\s+"))
       lines))

(defn load-circles [circle-path]
  (with-open [r (io/reader circle-path)]
    (doall (read-circles (line-seq r)))))

(defn read-feat-names [lines]
  (map (comp (partial str/join " ")
             rest
             #(str/split % #"\s+"))
       lines))

(defn load-feat-names [path]
  (with-open [r (io/reader path)]
    (doall (read-feat-names (line-seq r)))))
