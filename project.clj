(defproject othello "0.1.0-SNAPSHOT"
  :description "Learning Clojure by constructing Othello"
  :license {}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot othello.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
