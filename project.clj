(defproject sixtyfive "0.1.0-SNAPSHOT"

  :description "6502 emulator"

  :url "http://example.com/FIXME"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]]

  :main ^:skip-aot sixtyfive.core

  :target-path "target/%s"

  :profiles {:dev {:repl-options {:init-ns sixtyfive.core
                                  :port 6502}}
             
            :uberjar {:aot :all}  
             })
