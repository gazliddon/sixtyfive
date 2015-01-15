(ns sixtyfive.machine
  (:require [sixtyfive.cpu :as CPU]
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
  (get-pc [_ ])
  (set-pc [_ val])
  (get-opcode [_])

  (next-opcode [_ addr-mode])

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
  (set-pc [ this addr ]
    (.swap-cpu this #(CPU/set-pc %1 addr)))

  (get-pc [{:keys [cpu]}]
    (CPU/get-pc cpu))
  
  (get-opcode [this]
    (.read-byte this (.get-pc this)))

  (get-opcode-obj [this]
    (nth opcode-table (.get-opcode this)))

  (get-operand-addr [this]
    (inc (.get-pc this)) )
  
  (get-operand-byte[this] 
    (.read-byte this (.get-operand-addr this)))

  (get-operand-word[this] 
    (.read-word this (.get-operand-addr this)))

  (next-opcode [this addr-mode]
    (.set-pc this (+ 1 (.get-operand-size addr-mode) (.get-pc this))))

  (swap-cpu [this func]
    (assoc this :cpu (func (:cpu this))))  

  (swap-mem [this func]
    (assoc this :mem (func (:mem this)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Prg [^long address data])

(defn load-prg [^Machine {:keys [cpu] :as mac} ^Prg {:keys [address data] :as prg}]
  (-> mac
      (.swap-mem #(.write-block %1 address data))
      (.swap-cpu #(CPU/set-pc %1 address))))

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
                          (CPU/make-word))))

   :absolute-y   (reify IAddrMode
                   (get-operand-size [_]
                     2)
                   (calculate-address [_ {:keys [cpu] :as mac}]
                     (->> (.get-operand-word mac)
                          (+ (:Y cpu))
                          (CPU/make-word))))

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
(defn branch [^Machine {:keys [cpu] :as m} offset]
  (assert false))

(defn jump [^Machine m addr]
  (.set-pc m addr))

(defn fetch-word [^Machine m addr-mode]
  (let [addr (.calculate-address addr-mode m)]
    (.read-word m addr)))

(defn fetch-byte [^Machine m addr-mode]
  (let [addr (.calculate-address addr-mode m)]
    (.read-byte m addr)))


(defn glog [o]
  (println (str o))
  o
  )

(def opcode-factories
  [(reify
     OC/IOpCodeFactory
     (get-name [_] "JMP")

     (get-addr-modes [_]
       { 0x4c :absolute})

     (make-func [_ addr-mode]
       (fn [^Machine m]
         (->> (.calculate-address addr-mode m)
              (.set-pc m))))) 

   (reify
     OC/IOpCodeFactory
     (get-name [_] "INC")

     (get-addr-modes [_]
       {0x36 :zero-page   
        0xf6 :zero-page-x 
        0xee :absolute    
        0xfe :absolute-x})

     (make-func [_ addr-mode]
       (fn [^Machine machine]
         (let [addr (.calculate-address addr-mode machine)]
           ( -> 
             (->> (.read-byte machine addr)
                  (inc)
                  (.write-byte machine addr))
             (.next-opcode addr-mode))))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def op-code-table (OC/make-op-code-tab opcode-factories mode-to-addr-calc-func))

(defn mk-machine []
  (->Machine
    (CPU/mk-cpu)
    (M/mk-byte-memory 65536)
    op-code-table
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let's test it!
(def prg
  (->Prg
    0x1000
    [0xee 0xff 0x0f
     0x4c 0x00 0x10]))

(defn go [^Machine {:keys [cpu opcode-table] :as mac}]
  (let [pc (:PC cpu)
        opcode (.get-opcode-obj mac) ]
    (.exec-opcode opcode mac)))

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
      (.read-byte 0x1001)
      ))

(-> (mk-machine)
      (load-prg prg)
      (dump-cpu)
      (go)
      (dump-cpu)
      (go)
      (dump-cpu)
      (go)
      (.read-block 0xfff 10)
      )

