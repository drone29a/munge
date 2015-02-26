(ns munge.core
  (:require [schema.core :as s]))

(defn comb [& fns]
  (fn [& args]
    (vec (map (fn [f arg] (f arg)) fns args))))

(s/defn update-for-keys :- {s/Any s/Any}
  [m :- {s/Any s/Any}
   f :- (s/=> s/Any s/Any)
   ks :- [s/Any]
   init-v :- [s/Any]]
  (reduce (fn [acc-m k] (assoc acc-m k (f (get acc-m k init-v)))) m ks))

(s/defn randomly :- s/Any
  "Convenience function for randomly performing some side effect. Useful for
  simple probabilistic logging.

  Will always return the final argument x."
  [p :- s/Num
   f :- clojure.lang.IFn
   x :- s/Any]
  (if (> p (Math/random))
    (do (f x)
        x)
    x))
