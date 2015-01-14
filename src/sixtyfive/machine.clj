(ns sixtyfive.machine
  (:require [sixtyfive.cpu :refer :all]
            [sixtyfive.memory :as M]
            [sixtyfive.opcodes :as OC]
            )

  (:import [sixtyfive.cpu Cpu])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol IAddrMode
  (get-operand-size [_])
  (calculate-address [_ ^Machine m ]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Machine [^Cpu cpu mem opcode-table ]
  M/IMemory
  (read-byte [this addr]
    (.read-byte (:mem this) addr ))
  (write-byte [this addr val]
    (assoc this :mem (.write-byte (:mem this) this addr val )))
  (read-block [this addr size]
    (.read-block (:mem this) addr size ))
  (write-block [{:keys [mem] :as this} addr src]
    (let [new-mem (.write-block mem addr src)]
      (assoc this :mem new-mem))
    ))

(defn read-opcode [^Machine {:keys [cpu] :as mac}]
  (.read-byte mac (:PC cpu)))

(defn read-opcode-func [^Machine {:keys [opcode-table] :as mac}]
  (nth opcode-table (read-opcode mac)))

(defn read-operand-byte [^Machine {:keys [cpu] :as mac}]
  (.read-byte mac (get-operand-address cpu)))

(defn read-operand-word [^Machine {:keys [cpu] :as mac}]
  (.read-word mac (get-operand-address cpu)))

(defn go [^Machine {:keys [cpu opcode-table] :as mac}]
  (let [pc (:PC cpu)
        func (read-opcode-func mac)]
    (func mac)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Prg [^long address data])

(defn load-prg [^Machine {:keys [cpu] :as mac} ^Prg {:keys [address data] :as prg}]
  (-> mac
      (.write-block address data)
      (assoc :cpu (set-pc cpu address))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6502 addressing modes
(def mode-to-addr-calc-func
  {:immediate    (reify IAddrMode
                   (get-operand-size [_] 1)
                   (calculate-address [_  mac]
                     (read-operand-byte mac)))

   :zero-page    (reify IAddrMode
                   (get-operand-size [_] 1)
                   (calculate-address [_  mac]
                     (->> (read-operand-byte mac)
                          (.read-word mac))))

   :absolute     (reify IAddrMode
                   (get-operand-size [_] 2)
                   (calculate-address [_  mac]
                     (read-operand-byte mac)))

   :absolute-x   (reify IAddrMode
                   (get-operand-size [_] 2)
                   (calculate-address [_ {:keys [cpu] :as mac}]
                     (->> (read-operand-word mac)
                          (+ (:X cpu))
                          (make-word))))

   :absolute-y   (reify IAddrMode
                   (get-operand-size [_] 2)
                   (calculate-address [_ {:keys [cpu] :as mac}]
                     (->> (read-operand-word mac)
                          (+ (:Y cpu))
                          (make-word))))

   :zero-page-x  (reify IAddrMode
                   (get-operand-size [_] 1)
                   (calculate-address [_ mac]
                     (assert false)))      

   :zero-page-y  (reify IAddrMode
                   (get-operand-size [_] 1)
                   (calculate-address [_ mac]
                     (assert false)))

   :indirect-absolute  (reify IAddrMode
                         (get-operand-size [_] 2)
                         (calculate-address [_ mac]
                           (assert false)))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def op-code-factories
  [(reify OC/IOpCodeFactory
     (get-name [_] "JMP")

     (get-addr-modes [_]
       {0x4c :absolute    
        0x6c :absolute-indirect})

     (make-func [_ addr-mode]
       (fn [^Machine m]
         (assert false))))

   (reify OC/IOpCodeFactory
     (get-name [_] "INC")

     (get-addr-modes [_]
       {0x36 :zero-page   
        0xf6 :zero-page-x 
        0xee :absolute    
        0xfe :absolute-x})

     (make-func [_ addr-mode]
       (fn [^Machine m]
         (assert false)))) ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mk-machine []
  (->Machine
    (mk-cpu)
    (M/mk-byte-memory 65536)
    (OC/make-op-code-tab op-code-factories mode-to-addr-calc-func)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let's test it!
(def prg
  (->Prg
    0x4000
    [0xee 0x00 0x40    
     0x4c 0x00 0x10]))

(def mac 
  (->
    (mk-machine)
    (load-prg prg)
    ))

(class (:mem mac))

