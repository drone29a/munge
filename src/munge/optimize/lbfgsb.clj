(ns munge.optimize.lbfgsb
  (:require [schema.core :as sc]
            [schema.macros :as sm]
            [clojure.core.matrix :as mx])
  (:use [munge.schema :only [Vec Mat]])
  (:import (lbfgsb DifferentiableFunction 
                   FunctionValues
                   Bound
                   Minimizer)))

(sm/defn bound :- Bound
  [[l u] :- [(sc/one (sc/maybe Double) "l") (sc/one (sc/maybe Double) "u")]]
  (Bound. l u))

(sm/defn comm-minimizer :- Minimizer
  [bound-pairs :- [[(sc/one (sc/maybe Double) "lower") (sc/one (sc/maybe Double) "upper")]]]
  (doto (Minimizer.)
    (.setBounds (map bound bound-pairs))))

(sm/defn ps-and-l :- [(sc/one doubles "ps") (sc/one double "l")]
  [pt :- doubles]
  (let [num-dims (dec (count pt))
        dim-props (double-array num-dims pt)
        l-mult (aget pt num-dims)]
    [dim-props l-mult]))

;; TODO: note that we could mutate obj-mships so that we don't need to create
;; new closures at each iteration of algorithm. Probably bad idea, could
;; do the same for optimizing the obj memberships and doing both sounds
;; messy and possibly no savings?
(sm/defn f-del-prop :- Number
  "Calculte the partial derivative for a dimension in one of the 
  categorical distributions in the mix.
  Note that this is for the negative log-likelihood with Lagrange
  multiplier."
  [obj-mships :- Vec
   dim-props :- Vec
   comm-idx :- Integer
   dim-idx :- Integer
   num-obs :- Integer
   pt :- doubles]
  
  ;; NOTE: schema.macros/fn does not benefit from primitive types
  ;; (fn [^long num-obs
  ;;      ^doubles pt])

  (let [[iter-props l-mult] (ps-and-l pt)
        dim-iter-prop (mx/mget iter-props dim-idx)
        dim-iter-props (mx/mset dim-props comm-idx dim-iter-prop)]
    (double (+ (- (* num-obs (/ (mx/mget obj-mships comm-idx)
                                (mx/dot dim-iter-props obj-mships))))
               l-mult))))

(sm/defn f-del-l-mult :- Number
  "Calculate the partial derivative for the Lagrange multiplier in
  one of the categorical distributions in the mix."
  [pt :- doubles]
  (let [[iter-props _] (ps-and-l pt)]
    (double (- (reduce + iter-props) 1))))

(sm/defn f-dels-prop :- [(sm/=> double doubles)]
  "Create the set of f-del-prop fns, one for each dimension."
  [obj-mships :- Vec
   comm-props :- Mat
   comm-idx :- Integer
   obj-obs :- Vec]
  (map (fn [dim-idx dim-props num-obs] 
         (partial f-del-prop obj-mships dim-props comm-idx (int dim-idx) (int num-obs)))
       (range)
       (mx/columns comm-props)
       (mx/eseq obj-obs)))

(sm/defn f-neg-ll :- Number
  "Calculate the negative log-likelihood with Lagrange multiplier for a mixture of categoricals.
  Negative because we want to minimize."
  [obj-mships :- Vec
   num-obs :- Vec
   props :- Mat
   comm-idx :- Integer
   pt :- doubles]
  (let [[dim-props l-mult] (ps-and-l pt)
        updated-props (mx/set-row props comm-idx (vec dim-props))]
    (double (+ (- (mx/dot num-obs 
                          (mx/emap (fn [x] (Math/log (double x)))
                                   (mx/mmul obj-mships updated-props))))
               (* l-mult (- (mx/esum dim-props) 1))))))

(sm/defn h :- Number
  "Calculates the sum of squares of the gradient for the negative log-likelihood Lagrangian
  for a mixture of categoricals."
  [obj-mships :- Vec
   obj-obs :- Vec
   props :- Mat
   comm-idx :- Number
   pt :- doubles]
  (let [[dim-props l-mult] (ps-and-l pt)
        updated-props (mx/set-row props comm-idx (vec dim-props))]
    (double (+ (reduce + (map (fn [dim-idx dim-props num-obs]
                                (Math/pow (f-del-prop obj-mships dim-props comm-idx (int dim-idx) (int num-obs) pt)
                                          2))
                              (range)
                              (mx/columns props)
                              (mx/eseq obj-obs))
                       ;; (map (fn [f-del-prop-f]
                       ;;          (f-del-prop-f pt)) 
                       ;;        (f-dels-prop obj-mships props comm-idx obj-obs))
                       )
               (Math/pow (f-del-l-mult pt) 2)))))

(sm/defn h-del-prop :- Number
  "The partial derivative for the proportion components of the sum of squares objective."
  [obj-mship :- Number
   num-obs :- Number
   pt :- doubles]
  ;; NOTE: that perhaps obj-mship should never be 0, but just really small. 
  ;; NOTE: is there an optimization here to skip low weight memberships? 
  (double (+ (* 2 num-obs (/ 1 obj-mship)) 
             2)))

(sm/defn h-dels-prop :- [(sm/=> double doubles)]
  [obj-mships :- Vec
   obj-obs :- Vec]
  (map (fn [obj-mship num-obs]
         (partial h-del-prop obj-mship num-obs))
       (mx/eseq obj-mships)
       (mx/eseq obj-obs)))

(sm/defn h-del-l-mult :- Number
  [pt :- doubles]
  (let [num-dims (dec (count pt))]
    (double (* num-dims 2))))

(sm/defn approx-del :- Number
  [f :- (sm/=> double doubles)
   idx :- Number
   epsilon :- Double
   x :- doubles]
  (double (/ (- (f (doto (aclone x) 
                     (aset idx (+ (aget x idx) epsilon))))
                (f x))
             epsilon)))

(sm/defn obj-fn :- DifferentiableFunction
  "Create a DifferentiableFunction proxy for use
  with lbfgsb_wrapper."
  [f :- (sm/=> double doubles)
   dels :- [(sm/=> double doubles)]]
  (proxy [DifferentiableFunction] []
    (getValues [pt]
      (f pt)
      (FunctionValues. (f pt) (-> ((apply juxt dels) pt) double-array)))))

;; A test for l-bfgs-b wrapper
;; This works...
(comment (def q (proxy [DifferentiableFunction] []
                  (getValues [pt] 
                    (let [x (aget pt 0)]
                      (FunctionValues. (Math/pow (+ x 4) 2)
                                       (double-array [(* 2 (+ x 4))]))))))

         (def alg (doto (Minimizer.)
                    (.setBounds (map bound [[10.0 nil]]))))

         (def result (.run alg q (double-array [40]))))
