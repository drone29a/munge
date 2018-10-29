(ns munge.random
  (:require [clojure.core.typed :as t]
            [munge.struct.pair :refer [->OrderedPair]])
  (:import [java.util Random Collections]
           [munge.struct.pair OrderedPair]))

(t/defn :forall [a]
  random-nth
  "A rand-nth that uses a provided Random instance."
  [random :- Random
   xs :- (t/Coll a)] :- a
  (nth xs (.nextInt random (count xs))))

;; TODO: This could be generalized to a random-without-replacement fn,
;;       but not sure this is worthwhile as shuffle could be used for this purpose too.
(t/defn :forall [a]
  random-pair
  "Return a random pair of items from the given vector. The pair items will be unique."
  [random :- Random
   xs :- (t/Vec a)] :- (t/inst OrderedPair a)
  (let [x (random-nth random xs)
        y (->> (repeatedly #(random-nth random xs)) (drop-while (partial = x)) first)]
    (->OrderedPair x y)))

(t/defn :forall [a]
  shuffle
  [random :- Random
   xs :- (t/Coll a)]
  (let [xs* (java.util.ArrayList. xs)]
    (Collections/shuffle xs* random)
    (vec xs*)))
