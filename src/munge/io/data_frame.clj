(ns munge.io.data-frame
  (:require [clojure.string :refer [split join trim]]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [schema.core :as sc]
            [schema.macros :as sm]
            [schema.coerce :as s.coerce]))

(defn quoted-str [^String s] (and (.startsWith s "\"") (.endsWith s "\"")))

(def UnquotedStr (sc/pred #(and (string? %) (not (quoted-str %))) "unquoted-string"))
(def +unquoted-coercion+ {UnquotedStr (fn [^String s] (if (quoted-str s)
                                                (.substring s 1 (- (.length s) 1))
                                                s))})
(def +row-coercions+ (merge s.coerce/+string-coercions+
                            +unquoted-coercion+
                            {sc/Int (fn [^String x]
                                      (if (.equals "NA" x)
                                        -1
                                        ;;(s.coerce/edn-read-string x)
                                        (Integer/parseInt x)
                                        ))}))

(sm/defn read-data-line :- [sc/Str]
  [separator :- Character
   line :- sc/Str]
  (first (csv/read-csv line :separator separator)))

;; TODO: not resilient to multiple line records
;; TODO: return type is dependent on arguments
(sm/defn read-data-frame :- [[sc/Any]]
  [separator :- Character
   header? :- sc/Bool
   col-names :- [(sc/either sc/Str sc/Keyword)]
   row-schema :- sc/Schema
   rdr :- java.io.Reader]  
  (let [data (csv/read-csv rdr :separator separator)
        ;;data (for [l lines] (read-data-line separator l))
        num-cols (-> data first count)
        default-col-names (vec (for [i (range num-cols)] (format "col-%d" i)))
        ;; Schema must not be a lazy-seq
        unquoted-row-schema (vec (for [c-name default-col-names] (sc/one UnquotedStr c-name)))
        col-names (or col-names default-col-names)
        row-schema (or row-schema unquoted-row-schema)
        parse-row (s.coerce/coercer row-schema +row-coercions+)
        header (vec (map keyword (if header?
                                   ((s.coerce/coercer unquoted-row-schema +unquoted-coercion+) (first data))
                                   col-names)))
        rows (if header? (rest data) data)]
    (->> rows
         (map (comp vec parse-row))
         (vec))))

;; TODO: move to csv lib?
(defn write-data-frame [w headers records separator]
  (when headers
    (.write w (format "%s\n" (->> headers (map (partial format "\"%s\"")) (join separator)))))
  (doseq [r records]
    (.write w (format "%s\n" (->> r (map (partial format "\"%s\"")) (join separator))))))

;; returns [(sc/either {sc/Keyword sc/Str} [sc/Str])]
(defn load-data-frame
  "Load a data frame from the given file.
  Arguments:
    separator - the separator used
    header? - does the first line provide column names
    col-names - column names to use, will override header names
    row-schema - types for the columns, will return strings if not provided"
  [path & {:keys [separator header? col-names row-schema]
           :or {separator \,
                header? false
                col-names nil
                row-schema nil}}] 
  (with-open [r (io/reader path)]
    (read-data-frame separator header? col-names row-schema r)))

(defn save-data-frame [path headers records separator]
  (with-open [w (io/writer path)]
    (write-data-frame w headers records separator)))
