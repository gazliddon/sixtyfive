(ns sixtyfive.prg
  (:require [sixtyfive.protocols :as P]
            [sixtyfive.machine :as M])
  )


(defrecord Prg [^long address data])

(defn load-prg [^M/Machine mac ^Prg {:keys [address data]}]
  (.write-block (:mem mac) address data)
  )

