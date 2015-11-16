(ns munge.core
  (:require [schema.core :as s]
            [clojure.core.typed :as t]))

(defn comb [& fns]
  "Similar to juxt. Takes a collection of fns and returns a function which takes the 
  same number of args. Each arg is applied to one the fns. A vector of the results is returned.

  E.g., ((comb [inc dec name]) [1 43 :foo] -> [2 42 'foo']"
  
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

(t/defn get-private-field
  [x :- t/Any
   field :- t/Str] :- t/Any
   (let [^java.lang.reflect.Field f (-> ^java.lang.Object x
                                        (.getClass)
                                        (.getDeclaredField field))]
     (.setAccessible f true)
     (.get f x)))

;; TODO: feels like there's a more general way to represent this
(t/defn :forall [a]
  group-pairs
  "Creates a seq of pairs formed from partitioning (drop-last xs) and (rest xs)."
  [xs :- (t/Seq a)] :- (t/Seq (t/I (t/Seq a) (t/ExactCount 2)))
  (->> (interleave (drop-last xs) (rest xs))
       (partition 2)))
