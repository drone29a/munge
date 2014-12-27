(ns munge.core
  (:require [schema.core :as s]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix :as mx]
            [munge.schema :refer [Mat Vec]]
            [loom.graph :as lg]))

(defn comb [& fns]
  (fn [& args]
    (vec (map (fn [f arg] (f arg)) fns args))))

(s/defn update-for-keys :- {s/Any s/Any}
  [m :- {s/Any s/Any}
   f :- (s/=> s/Any s/Any)
   ks :- [s/Any]
   init-v :- [s/Any]]
  (reduce (fn [acc-m k] (assoc acc-m k (f (get acc-m k init-v)))) m ks))
