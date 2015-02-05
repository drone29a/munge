(ns munge.matrix-test
  (:require [clojure.test :refer :all]
            [munge.matrix :refer :all]
            [clojure.core.matrix :as mx]
            [schema.test]))

(use-fixtures :once schema.test/validate-schemas)

(mx/set-current-implementation :vectorz)

(deftest selected-rows-test
  (is (mx/equals (mx/matrix [[0 0 0]
                             [4 5 6]
                             [0 0 0]])
                 (selected-rows (mx/sparse (mx/matrix [[1 2 3]
                                                       [4 5 6]
                                                       [7 8 9]]))
                                (mx/matrix [0 1 0])))))

(deftest binary-vector-test
  (is (mx/equals (mx/matrix [1 1 0 0 1])
                 (binary-vector 5 [0 1 4]))))

(deftest b-or-test
  (is (mx/equals (binary-vector 5 [0 1 2 4])
                 (b-or (binary-vector 3 [0 1])
                       (binary-vector 5 [0 2])
                       (binary-vector 5 [4]))))
  (is (mx/equals (binary-vector 3 [0])
                 (b-or (binary-vector 3 [0])))))
