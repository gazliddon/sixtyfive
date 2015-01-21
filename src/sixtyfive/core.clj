(ns sixtyfive.core
  (:require [sixtyfive.machine :as M]
            [sixtyfive.prg :as P]
            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let's test it!
(def prg
  (->P/Prg
    0x1000
    [0xa9 0xff
     0xee 0xff 0x0f
     0xa9 0x00
     0x4c 0x00 0x10]))


(def mac (M/mk-machine))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



