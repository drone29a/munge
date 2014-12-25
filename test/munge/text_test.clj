(ns munge.text-test
  (:require [clojure.test :refer :all]
            [munge.text :refer :all]
            [schema.test]))

(use-fixtures :once schema.test/validate-schemas)

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
           (filter has-alpha? ["foo" "123" "<bar>"])))))

(deftest clean-terms-test
  (testing "removing trailing non-alphanum characters"
    (is (= ["foo" "123"]
           (map clean-trailing ["foo..." "123;"])))))
