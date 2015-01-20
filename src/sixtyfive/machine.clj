(ns sixtyfive.machine
  (:require [sixtyfive.cpu :as CPU]
            [sixtyfive.memory :as M]
            [sixtyfive.opcodes :as OC]
            [sixtyfive.utils :refer :all]
            [clojure.string :as STR]
            )

  (:import [sixtyfive.cpu Cpu])
  (:gen-class))

(set! *warn-on-reflection* true)
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
(defprotocol IAddrMode
  (get-str [_])
  (get-operand-size [_])
  (get-operand [_ ^Machine m addr])
  (calculate-address [_ ^Machine m ]))

(defn mk-diss-string [opcode operand]
  (let [op-str   (->> (OC/get-factory opcode)
                      (OC/get-name))
        addr-str (->> (OC/get-addr-mode opcode)
                      (get-str)) ]
    (-> addr-str
        (STR/replace "%1" op-str)
        (STR/replace "%2" (str operand)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol IMachine
  (get-pc [_ ])
  (set-pc [_ val])

  (set-reg [_ reg vcal])
  (get-reg [_ reg])

  (get-operand-byte [_])
  (get-operand-word [_])

  (get-opcode [_ addr])

  (disassemble [_ addr])

  (step [_])

  (swap-cpu [_ func]) 
  (swap-mem [_ func]))

(defrecord Machine [^Cpu cpu mem opcode-table ]

  )

(extend-type Machine
  M/IMemoryReader

  (read-word [{:keys [mem] } addr]
    (M/read-word mem addr))

  (read-byte [{:keys [mem] } addr]
    (M/read-byte mem addr) )

  (read-block [{:keys [mem] } src size]
    (M/read-block mem src size)  )

  M/IMemoryWriter
  (write-word [this addr v]
    (swap-mem this #(M/write-word %1 addr v)))

  (write-byte [this addr v]
    (swap-mem this #(M/write-byte %1 addr v)))

  (write-block [this dst src]
    (swap-mem this #(M/write-block %1 dst src)))
  )

(extend-type Machine
  IMachine
  (get-reg [{:keys [cpu]} reg]
    (CPU/get-reg cpu reg))

  (set-reg [this reg v]
    (swap-cpu this #(CPU/set-reg %1 reg v)))

  (set-pc [ this addr ]
    (swap-cpu this #(CPU/set-pc %1 addr)))

  (get-pc [{:keys [cpu]}]
    (CPU/get-pc cpu))

  (get-opcode [this]
    (assert false))

  (get-operand-word [this ]
    (M/read-word this (inc  (get-pc this))))

  (step [this]
    (let [pc (get-pc this)
          {:keys [opcode]} (get-opcode this pc)
          this' (OC/exec-opcode opcode this) ]
      this') )

  (disassemble [this addr]
    (let [{:keys [opcode operand]} (get-opcode this addr)]
      (mk-diss-string opcode operand )))

  (swap-cpu [this func]
    (assoc this :cpu (func (:cpu this))))  

  (swap-mem [this func]
    (assoc this :mem (func (:mem this))))  
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Prg [^long address data])

(defn load-prg [^Machine {:keys [cpu] :as mac} ^Prg {:keys [address data] :as prg}]
  (-> mac
      (swap-mem #(M/write-block %1 address data))
      (swap-cpu #(CPU/set-pc %1 address))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6502 addressing modes
(defn get-operand-addr [^Machine machine]
  (inc  (get-pc machine)))

(def mode-to-addr-calc-func
  {:immediate    (reify IAddrMode
                   (get-operand [_ machine addr] (M/read-byte machine (inc addr)))

                   (get-str [_] "%1 #%2")

                   (get-operand-size [_] 1)

                   (calculate-address [_  machine]
                     (get-operand-addr machine)))

   :zero-page    (reify IAddrMode
                   (get-operand [_ machine addr] (M/read-byte machine (inc addr)))

                   (get-str [_] "%1 %2")

                   (get-operand-size [_] 1)

                   (calculate-address [this  machine]
                     (->> (get-operand-byte machine)
                          (M/read-word machine))))

   :absolute     (reify IAddrMode
                   (get-operand [_ machine addr] (M/read-word machine (inc addr)))

                   (get-str [_] "%1 %2")

                   (get-operand-size [_] 2)

                   (calculate-address [this machine]
                     (get-operand-word machine)))

   :absolute-x   (reify IAddrMode
                   (get-operand [_ machine addr] (M/read-word machine (inc addr)))

                   (get-str [_] "%1 %2,X")

                   (get-operand-size [_] 2)

                   (calculate-address [this machine]
                     (->> (get-operand-word machine)
                          (+ (get-reg machine :X))
                          (CPU/make-word))))

   :absolute-y   (reify IAddrMode
                   (get-operand [_ machine addr] (M/read-word machine (inc addr)))

                   (get-str [_] "%1 %2,Y")

                   (get-operand-size [_] 2)

                   (calculate-address [this machine]
                     (->> (get-operand-word machine)
                          (+ (get-reg machine :Y))
                          (CPU/make-word))))

   :zero-page-x  (reify IAddrMode
                   (get-operand [_ machine addr] (M/read-byte machine (inc addr)))
                   (get-str [_] "%1 (%2,X)")
                   (get-operand-size [_] 1)
                   (calculate-address [_ mac]
                     (assert false)))      

   :zero-page-y  (reify IAddrMode
                   (get-operand [_ machine addr] (M/read-byte machine (inc addr)))
                   (get-str [_] "%1 (%2),Y")
                   (get-operand-size [_] 1)
                   (calculate-address [_ mac]
                     (assert false)))

   :absolute-indirect  (reify IAddrMode
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
       (+ 1 (get-operand-size addr-mode))
       (set-pc m)))

(defn fetch-word [^Machine m addr-mode]
  (let [addr (calculate-address addr-mode m)]
    (M/read-word m addr)))

(defn fetch-byte [^Machine m addr-mode]
  (let [addr (calculate-address addr-mode m)]
    (M/read-byte m addr)))



(defn fetch [^Machine machine addr-mode]
  (->>
    (calculate-address addr-mode machine )
    (M/read-byte machine)))

(defrecord OpCode [name addr-modes exec]
     OC/IOpCodeFactory
     (get-addr-modes [_] addr-modes)
     (get-name [_] name)
     (make-func [_ addr-mode]
       (fn [^Machine m]
         (exec m))))

(def opcode-factories
  [(reify
     OC/IOpCodeFactory
     (get-name [_] "JMP")

     (get-addr-modes [_] {0x4c :absolute
                          0x6c :absolute-indirect})

     (make-func [_ addr-mode]
       (fn [^Machine m]
         (->> (calculate-address addr-mode m)
              (jump m)))))

   (reify
     OC/IOpCodeFactory
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
     OC/IOpCodeFactory
     (get-name [_] "INC")

     (get-addr-modes [_]
       {0x36 :zero-page   
        0xf6 :zero-page-x 
        0xee :absolute    
        0xfe :absolute-x})

     (make-func [_ addr-mode]
       (fn [^Machine machine]
         (let [addr (calculate-address addr-mode machine)]
           (-> 
             (->> (M/read-byte machine addr)
                  (inc)
                  (M/write-byte machine addr))
             (next-instruction addr-mode))))))])

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
    [0xa9 0xff
     0xee 0xff 0x0f
     0xa9 0x00
     0x4c 0x00 0x10]))

(defn gpeek [^Machine m ^long addr]
  (M/read-byte m addr))

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

(defn finish [_]
  "Done"
  )

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














