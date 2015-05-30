(ns munge.coll-test
  (:require [clojure.test :refer :all]
            [munge.coll :refer :all]))

(deftest pair-test
  (are [x y] (= x y)
    (->Pair :a :b) (->Pair :b :a)
    :a (:x (->Pair :a :b))
    :b (:y (->Pair :a :b))))
