(ns munge.struct.pair-test
  (:require [clojure.test :refer :all]
            [munge.struct.pair :refer :all]))

(deftest pair-test
  (are [x y] (= x y)
    (->UnorderedPair :a :b) (->UnorderedPair :b :a)
    :a (:x (->UnorderedPair :a :b))
    :b (:y (->UnorderedPair :a :b))))
