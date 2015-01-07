(ns munge.graph-test
  (:require [clojure.test :refer :all]
            [munge.core :refer :all]
            [munge.graph :refer :all]
            [schema.test]
            [loom.graph :as lg]
            [loom.alg :as la]))
(use-fixtures :once schema.test/validate-schemas)

;; Graph with 3 maximal cliques: #{:a :b :c} #{:b :d :e} #{:e :f}
(def wg1 (lg/weighted-graph [:a :b 1]
                            [:a :c 1]
                            [:b :c 1]
                            [:b :d 1]
                            [:d :e 1]
                            [:b :e 1]
                            [:e :f 1]))

(deftest membership-graph-test
  (let [cliques [#{:e :b :d} #{:c :b :a} #{:e :f}]
        comms (into {} (vec (map-indexed (comb (comp keyword (partial format "comm-%s"))
                                               identity)
                                         cliques)))]
    (is (= #{#{:a :b :c}
             #{:b :d :e}
             #{:e :f}}
           (set cliques)))
    (is (= (-> (lg/weighted-graph)
               (lg/add-edges [:comm-0 :comm-1 0]
                             [:comm-0 :comm-2 0]
                             [:comm-1 :comm-0 0]
                             [:comm-1 :comm-2 1]
                             [:comm-2 :comm-0 0]
                             [:comm-2 :comm-1 1]))
           (membership-graph wg1 comms 4)))))

(deftest coincident-edges-test
  (let [ss [#{:a :b} #{:a :b :c} #{:b :c :d}]]
    (is (= {:b {:a 2, :c 2, :d 1},
            :a {:b 2, :c 1},
            :c {:b 2, :a 1, :d 1},
            :d {:c 1, :b 1}}
           (coincident-edges ss)))))

(deftest coincident-graph-test
  (let [ss [#{:a :b} #{:a :b :c} #{:b :c :d}]]
    (is (= (lg/weighted-graph {:b {:a 2, :c 2, :d 1},
                               :a {:b 2, :c 1},
                               :c {:b 2, :a 1, :d 1},
                               :d {:c 1, :b 1}})
           (coincident-graph ss)))))
