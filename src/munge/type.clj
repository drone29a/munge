(ns munge.type
  "A namespace for commong type annotations."
  (:require [clojure.core.typed :as t]
            [clojure.core.matrix.protocols :as mp]))

;; TODO: complete these. this is a placeholder so I can be begin using the type
(t/defalias Mat (t/I mp/PImplemenation mp/PQRDecomposition mp/PCholeskyDecomposition
                     mp/PSVDDecomposition mp/PNorm mp/PMatrixRank))

(t/defalias Vec t/Any)
