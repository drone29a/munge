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

(deftest l1-norm-test
  (is (= 1.0 (l1-norm (-> (mx/new-sparse-array [1000])
                          (mx/mset 0 0.2)
                          (mx/mset 1 -0.2)
                          (mx/mset 42 0.1)
                          (mx/mset 900 0.5)))))
  (is (= 30.0 (l1-norm (-> (mx/new-vector 1000)
                           (mx/mset 0 5)
                           (mx/mset 43 -10)
                           (mx/mset 613 0.5)
                           (mx/mset 999 14.5))))))

(deftest proportional-test
  (is (mx/equals (mx/matrix [0.3 0.3 0.4])
                 (proportional (mx/matrix [30 30 40])))))
