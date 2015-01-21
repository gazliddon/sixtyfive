(ns sixtyfive.prg
  (:require [sixtyfive.protocols :refer :all]))


(defrecord Prg [^Integer address data])

(defn load-prg [machine ^Prg {:keys [address data]}]
  (-> machine
    (write-block address data)
    (set-pc address)
    ))

