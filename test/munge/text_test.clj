(ns munge.text-test
  (:require [clojure.test :refer :all]
            [munge.text :refer :all]
            [munge.matrix :refer [sparse-matrix]]
            [clojure.core.matrix :as mx]
            [schema.test]))

(use-fixtures :once schema.test/validate-schemas)

;; TODO: This should be unnecessary, but Vectorz impl
;;       of equality doesn't work with other impls.
(mx/set-current-implementation :vectorz)

(defprotocol Close
  (close? [this other delta]
    "Check if two items of same type are numerically similar
    within some delta value."))
(extend-protocol Close
  java.lang.Number
  (close? [this other delta]
    (<= (Math/abs (- this other))
        delta))
  java.util.Map
  (close? [this other delta]
    (let [ks (keys this)]
      (and (= (set ks) (set (keys other)))
           (->> ks
                (map (fn [k]
                       (close? (get this k)
                               (get other k)
                               delta)))
                (every? true?)))))
  clojure.lang.Sequential
  (close? [this other delta]
    (every? (fn [[x y]]
              (close? x y delta))
            (map vector this other))))

(deftest preprocess-text
  (testing "converting markup to whitespace"
    (is (= "This is a test."
           (convert-xml-markup-to-whitespace "This<br>is<br/>a test."))))
  (testing "stripping markup"
    (is (= "This is a test."
           (strip-xml-markup "This is <span class=\"foo\">a</span> test.")))
    (is (= "This  is  a test.."
           (strip-tex-markup "This \\emph{is lost} is \\par a \\begin{frame}test.\\end{frame}.")))))

(deftest tokenization-test
  (testing "basic whitespace tokenization"
    (is (= ["These" "are," "42" "tokens." "not_really"]
           (tokenize "These are, 42 tokens.\nnot_really")))))

(deftest filter-test
  (testing "removing markup"
    (is (= ["foo" "123"]
           (remove xml-markup? ["foo" "<bar>" "123"])))
    (is (= ["foo" "123"]
           (remove tex-markup? ["foo" "\\begin{tabular}" "123"]))))
  (testing "keep tokens with alphabet characters"
    (is (= ["foo" "<bar>"]
           (filter has-alpha? ["foo" "123" "<bar>"]))))
  (testing "matching url-like strings"
    (is (url-like? "http://www.foo.com"))))

(deftest clean-terms-test
  (testing "removing trailing non-alphanum characters"
    (is (= ["foo" "123"]
           (map trim-non-alphanum ["foo..." "123;"]))))
  (testing "removing leading non-alphanum characters"
    (is (= ["foo" "123"]
           (map trim-non-alphanum ["((foo" "-123"]))))
  (testing "splitting on punctuation in token"
    (is (= ["basic" "stuff"]
           (split-punctuation "basic/stuff")))
    (is (= ["spl" "it" "it" "right" "d" "own" "the" "mid" "dle"]
           (split-punctuation "spl)it--it,right\\d.own/the-mid(dle)"))))
  (testing "making un-hyphenating hyphenated tokens"
    (is (= "somelongword"
           (unhyphenate "some-long-word")))
    (is (= "not--allofit"
           (unhyphenate "not--all-of-it")))))

(deftest extract-test
  (testing "extracting terms from a text string"
    (is (= ["brown" "fox" "wait" "number"]
           (extract-terms "The brown fox... wait, what? 123 - numbers?!")))))

(deftest frequency-test
  (testing "term frequencyn"
    (is (= {"a" 1 "b" 3 "ccc" 2}
           (tf ["ccc" "a" "b" "ccc" "b" "b"]))))
  (testing "term-document frequency"
    (is (= {"foo" 2 "bar" 1 "baz" 3}
           (tdf [{"foo" 1 "bar" 1 "baz" 1}
                 {"foo" 1 "baz" 1}
                 {"baz" 1}]))))
  (testing "inverse document frequency"
    (is (close? {"foo" 0.405
                 "bar" 1.098
                 "baz" 0}
                (idf [{"foo" 1 "bar" 1 "baz" 1}
                      {"foo" 1 "baz" 1}
                      {"baz" 1}])
                0.001)))
  (testing "term frequency-inverse document frequency"
    (is (close? [{"foo" 1.216 "bar" 2.197 "baz" 0}
                 {"foo" 0.405 "baz" 0}
                 {"baz" 0}]
                (tf-idf [["foo" "foo" "foo" "bar" "bar" "baz"]
                         (concat ["foo"] (repeat 10 "baz"))
                         ["baz"]])
                0.001))))

(deftest creation-test
  (testing "create doc-term matrix from doc-term maps"
    (is (mx/equals (mx/matrix [[1.0 0.0 2.0]
                               [3.0 2.0 0.0]
                               [1.0 3.0 0.0]])
                   (create-doc-term-matrix [{"foo" 2 "bar" 1}
                                            {"bar" 3 "baz" 2}
                                            {"bar" 1 "baz" 3}])))))
