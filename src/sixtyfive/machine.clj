(ns sixtyfive.machine
  (:require [sixtyfive.cpu :refer :all]
            [sixtyfive.memory :as M]
            [sixtyfive.opcodes :as OC]
            [sixtyfive.utils :refer :all]
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

  (write-byte [{:keys [mem] :as this} addr v]
    (let [mem' (.write-byte mem addr v)]
      (assoc this :mem mem')))

  (read-block [this addr size]
    (.read-block (:mem this) addr size ))

  (write-block [{:keys [mem] :as this} addr src]
    (let [mem' (.write-block mem addr src)]
      (assoc this :mem mem'))))

(defn read-opcode [^Machine {:keys [cpu] :as mac}]
  (.read-byte mac (:PC cpu)))

(defn read-opcode-func [^Machine {:keys [opcode-table] :as mac}]
  (nth opcode-table (read-opcode mac)))

(defn read-operand-byte [^Machine {:keys [cpu] :as mac}]
  (.read-byte mac (get-operand-address cpu)))

(defn read-lh [m addr]
  (.read-block m addr 2))

(defn read-word [m addr]
  (let [[l h] (read-lh m addr) ]
    (+ l (* h 0x100))))

(defn read-operand-word [^Machine {:keys [cpu] :as mac}]
  (let [pc (get-pc cpu) ]
    (read-word mac (inc pc) )))

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
                   (get-operand-size [_]
                     1)
                   (calculate-address [_  mac]
                     (read-operand-byte mac)))

   :zero-page    (reify IAddrMode
                   (get-operand-size [_]
                     1)
                   (calculate-address [_  mac]
                     (->> (read-operand-byte mac)
                          (.read-word mac))))

   :absolute     (reify IAddrMode
                   (get-operand-size [_]
                     2)
                   (calculate-address [_  mac]
                     (read-operand-word mac)))

   :absolute-x   (reify IAddrMode
                   (get-operand-size [_]
                     2)
                   (calculate-address [_ {:keys [cpu] :as mac}]
                     (->> (read-operand-word mac)
                          (+ (:X cpu))
                          (make-word))))

   :absolute-y   (reify IAddrMode
                   (get-operand-size [_]
                     2)
                   (calculate-address [_ {:keys [cpu] :as mac}]
                     (->> (read-operand-word mac)
                          (+ (:Y cpu))
                          (make-word))))

   :zero-page-x  (reify IAddrMode
                   (get-operand-size [_]
                     1)
                   (calculate-address [_ mac]
                     (assert false)))      

   :zero-page-y  (reify IAddrMode
                   (get-operand-size [_]
                     1)
                   (calculate-address [_ mac]
                     (assert false)))

   :absolute-indirect  (reify IAddrMode
                   (get-operand-size [_]
                     2)
                         (calculate-address [_ mac]
                           (assert false)))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn next-opcode [^Machine {:keys [cpu] :as m} addr-mode]
  (let [pc (get-pc cpu)
        pc' (+ 1 (.get-operand-size addr-mode))]
    (assoc m :cpu (set-pc cpu pc'))))

(def opcode-factories
  [ (reify OC/IOpCodeFactory
     (get-name [_] "INC")

     (get-addr-modes [_]
       {0x36 :zero-page   
        0xf6 :zero-page-x 
        0xee :absolute    
        0xfe :absolute-x})

     (make-func [_ addr-mode]
       (fn [^Machine m]
         (let [addr (.calculate-address addr-mode m)
               v (.read-byte m addr) ]
           (-> m
               (.write-byte addr (inc v))
               (next-opcode addr-mode)
               )))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def op-code-table (OC/make-op-code-tab opcode-factories mode-to-addr-calc-func))

(defn mk-machine []
  (->Machine
    (mk-cpu)
    (M/mk-byte-memory 65536)
    op-code-table
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let's test it!
(def prg
  (->Prg
    0x1000
    [0xee 0x00 0x01
     0x4c 0x00 0x10]))

(defn go [^Machine {:keys [cpu opcode-table] :as mac}]
  (let [pc (:PC cpu)
        opcode (read-opcode-func mac) ]
    (.exec-opcode opcode mac)
    ))

(defn pk [^Machine m ^long addr]
  (.read-byte m addr))

(def mac
  (-> (mk-machine)
      (load-prg prg)
      (go)
      (pk 0x100)
      ))




