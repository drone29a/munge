(ns munge.struct.pair
  "Protocol and implementations for ordered and unordered pairs of items."
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

(t/defprotocol [a] Pair
  (x [p] :- a "Return the item in the x slot.")
  (y [p] :- a "Return the item in the y slot."))

;; Unordered pairs
(t/ann-datatype [[a :variance :invariant]] UnorderedPair 
                [x :- a
                 y :- a])
;; TODO: Really want auto Keyword->field accessors for deftypes.
;;       Maybe add potemkin or a macro later?
(deftype UnorderedPair [x y]
  Object
  (equals [this other] (or (and (= (.x this) (.x other))
                                (= (.y this) (.y other)))
                           (and (= (.x this) (.y other))
                                (= (.y this) (.x other)))))
  (hashCode [this] (.hashCode ^clojure.lang.PersistentVector (sorted-pair x y)))
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
  Pair
  (x [p] (:x p))
  (y [p] (:y p)))

(t/ann-record [[a :variance :invariant]]
              OrderedPair
              [x :- a
               y :- a])
(defrecord OrderedPair
    [x y]
  Pair
  (x [p] (:x p))
  (y [p] (:y p)))
