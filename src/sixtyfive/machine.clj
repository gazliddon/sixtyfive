(ns sixtyfive.machine
  (:require [sixtyfive.cpu :refer [mk-cpu]]
            [sixtyfive.prg :as PRG]
            [sixtyfive.opcodes6502 :refer [get-opcode]]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mk-machine ^Machine []
  (->Machine
    (mk-cpu)
    (mk-byte-memory 65536)))

(def prg (PRG/make-prg-from-file "testbed/build/hmctest.bin"))

(defn opocode->operand-fetcher [opcode]
  (let [size 2]
    (get {1 (fn [_ _] nil)
          2 (fn [^Machine m] (read-byte m (inc (get-pc m))))
          3 (fn [^Machine m] (read-word m (inc (get-pc m))))} size))
  )

(defn decode-instruction [^Machine m ^Integer addr]
  (let [opcode-hex (read-byte m addr)
        opcode (get-opcode opcode-hex)
        operand-fetcher (opocode->operand-fetcher  opcode)
        ]
    {:opcode-hex opcode-hex
     :opcode opcode
     :operand-fetcher operand-fetcher}))

(defn decode-instruction->pc [^Machine m]
  (->> (get-pc m)
       (decode-instruction m)))

(-> (mk-machine)
    (PRG/load-prg prg)
    (decode-instruction->pc))


