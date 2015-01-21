(ns sixtyfive.machine
  (:require [sixtyfive.cpu :as CPU]
            [sixtyfive.memory :as M]
            [sixtyfive.opcodes :as OC]
            [sixtyfive.utils :refer :all]
            [clojure.string :as STR]
            [sixtyfive.protocols :as P]
            )

  (:import [sixtyfive.memory ByteMemory]
           [sixtyfive.cpu Cpu]))


(set! *warn-on-reflection* true)

(defprotocol IMachine
  (get-pc [_ ])
  (set-pc [_ ^long val])

  (set-reg [_ reg vcal])
  (get-reg [_ reg])

  (get-operand-byte [_])
  (get-operand-word [_])

  (get-opcode [_ ^long addr])

  (disassemble [_ ^long addr])

  (step [_])

  (swap-cpu [_ func]) 
  (swap-mem [_ func]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell style flip
(defn flip [function] 
  (fn 
    ([] (function))
    ([x] (function x)) 
    ([x y] (function y x)) 
    ([x y z] (function z y x)) 
    ([a b c d] (function d c b a)) 
    ([a b c d & rest]
        (->> rest
            (concat [a b c d])
            reverse
            (apply function)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mk-diss-string [opcode operand]
  (let [op-str   (->> (P/get-factory opcode)
                      (P/get-name))
        addr-str (->> (P/get-addr-mode opcode)
                      (P/get-str)) ]
    (-> addr-str
        (STR/replace "%1" op-str)
        (STR/replace "%2" (str operand)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Machine [^Cpu cpu ^ByteMemory mem opcode-table])

(extend-protocol P/IMemoryWriter
  Machine
  (write-word [this addr v]
    (swap-mem this #(P/write-word %1 addr v)))

  (write-byte [this addr v]
    (swap-mem this #(P/write-byte %1 addr v)))

  (write-block [this dst src]
    (assoc this :mem (P/write-block (:mem this) dst src))))

(extend-protocol P/IMemoryReader
  Machine
  (read-word [{:keys [mem] } addr]
    (P/read-word mem addr))

  (read-byte [{:keys [mem] } addr]
    (P/read-byte mem addr))

  (read-block [{:keys [mem] } src size]
    (P/read-block mem src size))
  )

(extend-protocol IMachine
  Machine
  (get-reg [{:keys [cpu]} reg]
    (CPU/get-reg cpu reg))

  (set-reg [this reg v]
    (swap-cpu this #(CPU/set-reg %1 reg v)))

  (set-pc [ this addr ]
    (swap-cpu this #(CPU/set-pc %1 addr)))

  (get-pc [{:keys [cpu]}]
    (CPU/get-pc cpu))

  (get-opcode [this addr]
    (let [hex (M/read-byte this addr)
          opcode (nth hex opcode-table)
          addr-mode (:addressing-mode opcode)
          operand (get-operand addr-mode this addr)]
      {:opcode opcode
       :operand operand}))

  (get-operand-word [this ]
    (P/read-word this (inc  (get-pc this))))

  (step [this]
    (let [pc (get-pc this)
          {:keys [opcode]} (get-opcode this pc)
          this' (P/exec-opcode opcode this) ]
      this') )

  (disassemble [this addr]
    (let [{:keys [opcode operand]} (get-opcode this addr)]
      (mk-diss-string opcode operand )))

  (swap-cpu [this func]
    (assoc this :cpu (func (:cpu this))))  

  (swap-mem [this func]
    (assoc this :mem (func (:mem this))))  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Prg [^long address data])

(defn load-prg [^Machine mac ^Prg {:keys [address data]}]
  (println (class mac))
  (println (class (:mem mac)))
  (println "ABOUT TO WRITE BLOCK load-prg")
  (M/write-block (:mem mac) address data)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6502 addressing modes
(defn get-operand-addr [^Machine machine]
  (inc  (get-pc machine)))

(def mode-to-addr-calc-func
  {:immediate    (reify P/IAddrMode
                   (get-operand [_ machine addr] (P/read-byte machine (inc addr)))

                   (get-str [_] "%1 #%2")

                   (get-operand-size [_] 1)

                   (calculate-address [_  machine]
                     (get-operand-addr machine)))

   :zero-page    (reify P/IAddrMode
                   (get-operand [_ machine addr] (P/read-byte machine (inc addr)))

                   (get-str [_] "%1 %2")

                   (get-operand-size [_] 1)

                   (calculate-address [this  machine]
                     (->> (get-operand-byte machine)
                          (P/read-word machine))))

   :absolute     (reify P/IAddrMode
                   (get-operand [_ machine addr] (P/read-word machine (inc addr)))

                   (get-str [_] "%1 %2")

                   (get-operand-size [_] 2)

                   (calculate-address [this machine]
                     (get-operand-word machine)))

   :absolute-x   (reify P/IAddrMode
                   (get-operand [_ machine addr] (P/read-word machine (inc addr)))

                   (get-str [_] "%1 %2,X")

                   (get-operand-size [_] 2)

                   (calculate-address [this machine]
                     (->> (get-operand-word machine)
                          (+ (get-reg machine :X))
                          (CPU/make-word))))

   :absolute-y   (reify P/IAddrMode
                   (get-operand [_ machine addr] (P/read-word machine (inc addr)))

                   (get-str [_] "%1 %2,Y")

                   (get-operand-size [_] 2)

                   (calculate-address [this machine]
                     (->> (get-operand-word machine)
                          (+ (get-reg machine :Y))
                          (CPU/make-word))))

   :zero-page-x  (reify P/IAddrMode
                   (get-operand [_ machine addr] (P/read-byte machine (inc addr)))
                   (get-str [_] "%1 (%2,X)")
                   (get-operand-size [_] 1)
                   (calculate-address [_ mac]
                     (assert false)))      

   :zero-page-y  (reify P/IAddrMode
                   (get-operand [_ machine addr] (P/read-byte machine (inc addr)))
                   (get-str [_] "%1 (%2),Y")
                   (get-operand-size [_] 1)
                   (calculate-address [_ mac]
                     (assert false)))

   :absolute-indirect  (reify P/IAddrMode
                   (get-operand [_ machine addr] (get-operand-word machine))
                   (get-str [_] "%1 (%2)")
                   (get-operand-size [_] 2)
                   (calculate-address [_ mac]
                     (assert false)))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn branch [^Machine {:keys [cpu] :as m} offset]
  (assert false))

(defn jump [^Machine m addr]
  (set-pc m addr))

(defn next-instruction [^Machine m addr-mode]
  (->> (get-pc m)
       (+ 1 (P/get-operand-size addr-mode))
       (set-pc m)))

(defn fetch-word [^Machine m addr-mode]
  (let [addr (P/calculate-address addr-mode m)]
    (P/read-word m addr)))

(defn fetch-byte [^Machine m addr-mode]
  (let [addr (P/calculate-address addr-mode m)]
    (P/read-byte m addr)))


(defn fetch [^Machine machine addr-mode]
  (->>
    (P/calculate-address addr-mode machine )
    (P/read-byte machine)))

(defrecord OpCode [name addr-modes exec]
     P/IOpCodeFactory
     (get-addr-modes [_] addr-modes)
     (get-name [_] name)
     (make-func [_ addr-mode]
       (fn [^Machine m]
         (exec m))))

(def opcode-factories
  [(reify
     P/IOpCodeFactory
     (get-name [_] "JMP")

     (get-addr-modes [_] {0x4c :absolute
                          0x6c :absolute-indirect})

     (make-func [_ addr-mode]
       (fn [^Machine m]
         (->> (P/calculate-address addr-mode m)
              (jump m)))))

   (reify
     P/IOpCodeFactory
     (get-name [_] "LDA")

     (get-addr-modes [_]
       {0xa9 :immediate})

     (make-func [_ addr-mode]
       (fn [^Machine machine]
         (->
           (->>
             (fetch machine addr-mode)
             (set-reg machine :A))
           (next-instruction addr-mode)))))

   (reify
     P/IOpCodeFactory
     (get-name [_] "INC")

     (get-addr-modes [_]
       {0x36 :zero-page   
        0xf6 :zero-page-x 
        0xee :absolute    
        0xfe :absolute-x})

     (make-func [_ addr-mode]
       (fn [^Machine machine]
         (let [addr (P/calculate-address addr-mode machine)]
           (-> 
             (->> (P/read-byte machine addr)
                  (inc)
                  (P/write-byte machine addr))
             (next-instruction addr-mode))))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def op-code-table (OC/make-op-code-tab opcode-factories mode-to-addr-calc-func))

(defn mk-machine ^Machine []
  (->Machine
    (CPU/mk-cpu)
    (M/mk-byte-memory 65536)
    op-code-table
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let's test it!
(def prg
  (Prg.
    0x1000
    [0xa9 0xff
     0xee 0xff 0x0f
     0xa9 0x00
     0x4c 0x00 0x10]))

(defn gpeek [^Machine m ^long addr]
  (P/read-byte m addr))

(defn dump-cpu [^Machine {:keys [cpu] :as m}]
  (let [cpu-str (str cpu)
        pc (get-pc m)
        ]
    (println cpu)
    m))  

(defn diass [^Machine m]
  (let [pc (get-pc m)
        diss (disassemble m (get-pc m))]
    (println (str pc ": " diss))
    m
    )
  )

(defn finish [_] "Done"
  

(def mac (mk-machine))

(comment -> (mk-machine)
    (load-prg prg)
    (diass)
    (step)
    (diass)
    (step)
    (diass)
    (step)
    (diass)
    (step)
    (diass)
    (finish))














