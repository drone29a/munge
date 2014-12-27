(ns munge.text
  (:refer-clojure :exclude [replace])
  (:require [schema.core :as s]
            [munge.core :as m]
            [munge.schema :refer [Mat]]
            [stemmers.porter :refer [stem]]
            [clojure.string :refer [split replace lower-case]]
            [munge.matrix :refer [sparse-matrix]]))

;; Need functions for:
;; tokenization
;; stopword filter
;; tf-idf calculation

(declare stopwords)

(s/defn tokenize :- [s/Str]
  "Find word-like tokens in a string by splitting on whitespace."
  [text :- s/Str]
  (->> (split text #"\s+")))

(s/defn has-alpha? :- s/Bool
  [token :- s/Str]
  (->> token
       (re-find #"[a-zA-Z]")
       nil?
       not))

(s/defn xml-markup? :- s/Bool
  [token :- s/Str]
  (->> token
       (re-find #"^\<.*\>$")
       nil?
       not))

(s/defn tex-markup? :- s/Bool
  [token :- s/Str]
  (->> token
       (re-find #"^\\[\{\}\[\]\(\)\w]+\W?$")
       nil?
       not))

(s/defn clean-trailing :- s/Str
  [token :- s/Str]
  (replace token #"\W+$" ""))

(s/defn stopword? :- s/Bool
  [token :- s/Str]
  (contains? stopwords token))

(s/defn extract-terms :- [s/Str]
  [text :- s/Str]
  (->> text
       tokenize
       (filter has-alpha?)
       (remove xml-markup?)
       (remove tex-markup?)
       (map clean-trailing)
       (map lower-case)
       (remove stopword?)
       (map stem)))

(s/defn tf :- {s/Str s/Int}
  "Calculate term frequency."
  [terms :- [s/Str]]
  (reduce (fn [freqs term]
            (update-in freqs [term] (fnil inc 0)))
          {}
          terms))

(s/defn tdf :- {s/Str s/Int}
  "Finds how many documents each term occurs."
  [docs :- [{s/Str s/Int}]]
  (reduce (fn [freqs doc]
            (loop [fs freqs
                   terms (keys doc)]
              (if (empty? terms)
                fs
                (recur (update-in fs [(first terms)] (fnil inc 0))
                       (rest terms)))))
          {}
          docs))

(s/defn idf :- {s/Str s/Num}
  "Calculate the inverse document frequency."
  [docs :- [{s/Str s/Int}]]
  (let [term-doc-freq (tdf docs)
        num-docs (count docs)]
    (into {} (map (fn [[term freq]]
                    [term (Math/log (/ num-docs freq))])
                  term-doc-freq))))

(s/defn tf-idf :- [{s/Str s/Num}]
  [term-seqs :- [[s/Str]]]
  (let [docs (map tf term-seqs)
        inv-doc-freq (idf docs)
        terms (keys inv-doc-freq)]
    (map (fn [d]
           (->> (for [t (keys d)]
                  [t (* (get d t) (get inv-doc-freq t))])
                (into {})))
         docs)))

(s/defn create-doc-term-matrix :- Mat
  ([doc-terms :- [{s/Str s/Num}]]
     (create-doc-term-matrix (->> doc-terms
                                  (mapcat keys)
                                  set)
                             doc-terms))
  ([terms :- #{s/Str}
    doc-terms :- [{s/Str s/Num}]]
     (let [num-docs (count doc-terms)
           num-terms (count terms)
           term-index (->> terms
                         sort
                         (map-indexed (comp vec reverse vector))
                         (into {}))]
       (sparse-matrix num-docs
                      num-terms
                      (for [dt doc-terms]
                        (->> dt
                             (map (partial apply (m/comb term-index identity)))
                             (into {})))))))

(s/defn process-corpus :- Mat
  "Generate tf-idf matrix for a set of documents."
  [docs :- [s/Str]]
  (->> docs
       (map extract-terms)
       (tf-idf)
       (create-doc-term-matrix)))

;; From http://www.ranks.nl/stopwords
(s/def stopwords :- #{s/Str}
  #{"a"
    "about"
    "above"
    "after"
    "again"
    "against"
    "all"
    "am"
    "an"
    "and"
    "any"
    "are"
    "aren't"
    "as"
    "at"
    "be"
    "because"
    "been"
    "before"
    "being"
    "below"
    "between"
    "both"
    "but"
    "by"
    "can't"
    "cannot"
    "could"
    "couldn't"
    "did"
    "didn't"
    "do"
    "does"
    "doesn't"
    "doing"
    "don't"
    "down"
    "during"
    "each"
    "few"
    "for"
    "from"
    "further"
    "had"
    "hadn't"
    "has"
    "hasn't"
    "have"
    "haven't"
    "having"
    "he"
    "he'd"
    "he'll"
    "he's"
    "her"
    "here"
    "here's"
    "hers"
    "herself"
    "him"
    "himself"
    "his"
    "how"
    "how's"
    "i"
    "i'd"
    "i'll"
    "i'm"
    "i've"
    "if"
    "in"
    "into"
    "is"
    "isn't"
    "it"
    "it's"
    "its"
    "itself"
    "let's"
    "me"
    "more"
    "most"
    "mustn't"
    "my"
    "myself"
    "no"
    "nor"
    "not"
    "of"
    "off"
    "on"
    "once"
    "only"
    "or"
    "other"
    "ought"
    "our"
    "ours"
    "ourselves"
    "out"
    "over"
    "own"
    "same"
    "shan't"
    "she"
    "she'd"
    "she'll"
    "she's"
    "should"
    "shouldn't"
    "so"
    "some"
    "such"
    "than"
    "that"
    "that's"
    "the"
    "their"
    "theirs"
    "them"
    "themselves"
    "then"
    "there"
    "there's"
    "these"
    "they"
    "they'd"
    "they'll"
    "they're"
    "they've"
    "this"
    "those"
    "through"
    "to"
    "too"
    "under"
    "until"
    "up"
    "very"
    "was"
    "wasn't"
    "we"
    "we'd"
    "we'll"
    "we're"
    "we've"
    "were"
    "weren't"
    "what"
    "what's"
    "when"
    "when's"
    "where"
    "where's"
    "which"
    "while"
    "who"
    "who's"
    "whom"
    "why"
    "why's"
    "with"
    "won't"
    "would"
    "wouldn't"
    "you"
    "you'd"
    "you'll"
    "you're"
    "you've"
    "your"
    "yours"
    "yourself"
    "yourselves"})
