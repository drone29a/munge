(ns munge.io.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.matrix :as mx]))

(defn load-ids [path]
  (with-open [r (io/reader path)]
    (vec (doall (map (fn [row]
                       (vec (map #(Integer/parseInt %) row)))
                     (map #(str/split % #",") (line-seq r)))))))

(defn line-to-list [line]
  "Produce an adj list from a space-separate string of values."
  (filter #(not (zero? (second %)))
          (map-indexed (fn [idx x] [idx (Double/parseDouble x)]) (filter #(not (empty? %)) (str/split line #"\s+")))))

(defn load-adjacency-lists [lines]
  (map (fn [adj-list]
         (into {} (line-to-list adj-list))) lines))

(defn load-term-doc [lines tdm]
  "Loads a term-doc matrix from space-delimited row lines."
  (let [num-rows (mx/row-count tdm)
        num-cols (mx/column-count tdm)]
    (doseq [[row-idx l] (map vector (range num-rows) lines)]
      (doseq [[col-idx x] (line-to-list l)]
        (mx/mset! tdm row-idx col-idx x))))
  tdm)

(defn load-index [lines]
  (map identity lines))
