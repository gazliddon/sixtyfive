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
(defprotocol IMachine
  (get-opcode [_])
  (get-opcode-obj [_])
  (get-operand-addr [_])
  (get-operand-byte [_])
  (get-operand-word [_])
  (swap-cpu [_ func]) 
  (swap-mem [_ func]))

(defrecord Machine [^Cpu cpu mem opcode-table ]

  M/IMemoryReader
  (read-word [{:keys [mem] } addr]
    (.read-word mem addr))

  (read-byte [{:keys [mem] } addr]
    (.read-byte mem addr) )

  (read-block [{:keys [mem] } src size]
    (.read-block mem src size)  )

  M/IMemoryWriter
  (write-word [this addr v]
    (swap-mem this #(.write-word %1 addr v)))

  (write-byte [this addr v]
    (swap-mem this #(.write-byte %1 addr v)))

  (write-block [this dst src]
    (swap-mem this #(.write-block %1 dst src)))

  IMachine
  (get-opcode [this]
    (.read-byte this (get-pc cpu)))

  (get-opcode-obj [this]
    (nth opcode-table (get-opcode this)))

  (get-operand-addr [{:keys [cpu] :as this}]
    (.read-byte this (inc (get-pc cpu))) )
  
  (get-operand-byte[m] 
    (.read-byte m (get-operand-addr m)))

  (get-operand-word[m] 
    (.read-word m (get-operand-addr m)))

  (swap-cpu [this func]
    (assoc this :cpu (func (:cpu this))))  

  (swap-mem [this func]
    (assoc this :mem (func (:mem this)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Prg [^long address data])

(defn load-prg [^Machine {:keys [cpu] :as mac} ^Prg {:keys [address data] :as prg}]
  (-> mac
      (.swap-mem #(.write-block %1 address data))
      (.swap-cpu #(set-pc %1 address))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6502 addressing modes


(def mode-to-addr-calc-func
  {:immediate    (reify IAddrMode
                   (get-operand-size [_]
                     1)
                   (calculate-address [_  mac]
                     (.get-operand-byte mac)))

   :zero-page    (reify IAddrMode
                   (get-operand-size [_]
                     1)
                   (calculate-address [_  {:keys [mem] :as m}]
                     (->> (.get-operand-byte m)
                          (.read-word mem))))

   :absolute     (reify IAddrMode
                   (get-operand-size [_]
                     2)
                   (calculate-address [_  m]
                     (.get-operand-word m)))

   :absolute-x   (reify IAddrMode
                   (get-operand-size [_]
                     2)
                   (calculate-address [_ {:keys [cpu] :as mac}]
                     (->> (.get-operand-word mac)
                          (+ (:X cpu))
                          (make-word))))

   :absolute-y   (reify IAddrMode
                   (get-operand-size [_]
                     2)
                   (calculate-address [_ {:keys [cpu] :as mac}]
                     (->> (.get-operand-word mac)
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
        pc' (+ pc 1 (.get-operand-size addr-mode))]
    (assoc m :cpu (set-pc cpu pc'))))

(defn branch [^Machine {:keys [cpu] :as m} offset]
  (assert false))

(defn jump [^Machine {:keys [cpu] :as m} addr]
  (println "GOT TO JUMP1")
  (assert false))

(defn fetch-word [^Machine m addr-mode]
  (let [addr (.calculate-address addr-mode m)]
    (.read-word m addr)))

(defn fetch-byte [^Machine m addr-mode]
  (let [addr (.calculate-address addr-mode m)]
    (.read-byte m addr)))

(def opcode-factories
  [(reify
     OC/IOpCodeFactory
     (get-name [_] "JMP")

     (get-addr-modes [_]
       { 0x4c :absolute})

     (make-func [_ addr-mode]
       (fn [^Machine m]
         (->> (.calculate-address addr-mode m)
              (.read-word m )
              (jump m))))) 

   (reify
     OC/IOpCodeFactory
     (get-name [_] "INC")

     (get-addr-modes [_]
       {0x36 :zero-page   
        0xf6 :zero-page-x 
        0xee :absolute    
        0xfe :absolute-x})

     (make-func [_ addr-mode]
       (fn [^Machine m]
         
         (let [addr (.calculate-address addr-mode m)
               v (.read-byte m addr)
               m' (.write-byte m addr (inc v))]
           m'
           
           ))))])

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
        opcode (.get-opcode-obj mac) ]
    (.exec-opcode opcode mac)
    ))

(defn gpeek [^Machine m ^long addr]
  (.read-byte m addr))

(defn dump-cpu [^Machine {:keys [cpu] :as m}]
  (println cpu)
  m
  )  

(def mac
  (-> (mk-machine)
      (load-prg prg)
      (dump-cpu)
      (go)
      (dump-cpu)
      (gpeek 0x100)
      ))



