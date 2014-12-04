(ns munge.data-frame)

(comment
  ;; We should be able to use column names to fetch column
  (:x df)
  ;; or
  (df :x)

  ;; We should be able to use nth to get a row
  (nth df idx)
  ;; or
  (df idx)
  
  )
 

(defprotocol PDataFrame
  "A tabular data frame."
  )

(deftype Table
    PDataFrame)
