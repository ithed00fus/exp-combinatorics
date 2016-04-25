(defproject zombie-apocalypse "0.1.0-SNAPSHOT"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter "1.5.5"]]
  :main ^:skip-aot zombie-apocalypse.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
                                
