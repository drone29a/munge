(defproject munge "0.1.3-SNAPSHOT"
  :description "Support for data processing and anaylsis."
  :url "https://github.com/mattrepl/munge"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.typed "0.3.18"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/data.csv "0.1.2"]
                 [net.mikera/core.matrix "0.45.0"]
                 [net.mikera/vectorz-clj "0.37.0"]
                 [prismatic/schema "1.0.3"]
                 [aysylu/loom "0.5.4"]
                 [stemmers "0.2.2"]
                 ;; TODO: Move to separate lib?
                 ;; [mkobos/lbfgsb-wrapper "1.1.3-SNAPSHOT" :native-prefix ""]
                 ]
  :native-path "native")
