(ns sixtyfive.machine
  (:require [sixtyfive.cpu :refer [mk-cpu]]
            [sixtyfive.memory :refer [mk-byte-memory]]
            [sixtyfive.protocols :refer :all]))

(set! *warn-on-reflection* true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Machine [cpu mem]
  IMemoryWriter
  (write-word [this dst v]
    (assoc this :mem (write-word mem dst v)) )

  (write-byte [this dst v]
    (assoc this :mem (write-byte mem dst v)))

  (write-block [this dst src]
    (assoc this :mem (write-block mem dst src)))

  IMemoryReader
  (read-byte [_ src]
    (read-byte mem src))

  (read-word [_ src]
    (read-word mem src))

  (read-block [_ src len]
    (read-block mem src len))

  IMachine
  (set-pc [ this addr ]
    (assoc-in this [:cpu :PC] addr))

  (get-pc [this]
    (:PC cpu)))


(defn mk-machine ^Machine []
  (->Machine
    (mk-cpu)
    (mk-byte-memory 10)))











