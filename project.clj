(defproject munge "0.1.1"
  :description "Support for data processing and anaylsis."
  :url "https://github.com/mattrepl/munge"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.typed "0.3.7"]
                 [org.clojure/data.csv "0.1.2"]
                 [net.mikera/core.matrix "0.34.1-SNAPSHOT"]
                 [net.mikera/vectorz-clj "0.29.1-SNAPSHOT"]
                 [prismatic/schema "0.4.3"]
                 [aysylu/loom "0.5.1-SNAPSHOT"]
                 [stemmers "0.2.2"]
                 ;; TODO: Move to separate lib?
                 ;; [mkobos/lbfgsb-wrapper "1.1.3-SNAPSHOT" :native-prefix ""]
                 ]
  :native-path "native")
