(ns munge.coll
  (:require [clojure.core.typed :as t])
  (:import [clojure.lang ILookup IPersistentSet]))

;; TODO: Could add filter that marks sorted relation in result.
(t/defn :forall [a]
  sorted-pair
  [x :- a
   y :- a] :- '[a a]
   (if (<= (compare x y) 0)
     [x y]
     [y x]))

;; Unordered pairs
(t/ann-record [[a :variance :invariant]] Pair 
              [x :- a
               y :- a])
;; TODO: Really want auto Keyword->field accessors for deftypes.
;;       Maybe add potemkin or a macro later?
(deftype Pair [x y]
  Object
  (equals [this other] (or (and (= (.x this) (.x other))
                                (= (.y this) (.y other)))
                           (and (= (.x this) (.y other))
                                (= (.y this) (.x other)))))
  (hashCode [this] (.hashCode (sorted-pair x y)))
  (toString [this] (str (sorted-pair x y)))
  ILookup
  (valAt [this key]
    (case key
      :x x
      :y y
      nil))
  (valAt [this key not-found]
    (or (.valAt this key) not-found))
  ;; TODO: Wanted this for contains? support, but ends up causing an error when checking
  ;;       for equality.
  ;; IPersistentSet
  ;; (contains [this key]
  ;;   (or (= x key) (= y key)))
  ;; (disjoin [this key]
  ;;   (throw (UnsupportedOperationException. "Function not supported for Pair type.")))
  ;; (get [this key]
  ;;   (throw (UnsupportedOperationException. "Function not supported for Pair type.")))
  )
