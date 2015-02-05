(ns munge.core-test
  (:require [clojure.test :refer :all]
            [munge.core :refer :all]))

(defn close? [delta-thresh expected actual]
  (< (Math/abs (- expected actual)) delta-thresh))

(defn close-seq? [delta-thresh expected actual]
  (every? (fn [[e a]] (< (Math/abs (- e a)) delta-thresh))
          (map vector expected actual)))
