(ns sixtyfive.opcodes6502
  (:require [sixtyfive.protocols :refer :all]
            [sixtyfive.utils     :refer [mk-vec]]
            [sixtyfive.cpu       :refer [get-v
                                         get-c
                                         get-n
                                         get-z
                                         set-v
                                         set-c
                                         set-n
                                         set-z]]
            ))

(def addressing-modes
  {:absolute     {:help-text   ""
                  :disassembly "%1 %2"
                  :size        3
                  }

   :absolute-x   {:help-text   ""
                  :disassembly "%1 %2,X"
                  :size        3}

   :accumulator  {:help-text   ""
                  :disassembly "%1 A"
                  :size        2}

   :immediate    {:help-text   ""
                  :disassembly "%1 #%2"
                  :size        2}

   :acccumalator {:help-text ""
                  :disassembly "%1"
                  :size 1}


   :x-register   {:help-text ""
                  :disassembly "%1"
                  :size 1}

   :y-register   {:help-text ""
                  :disassembly "%1"
                  :size 1 }

   :implied      {:help-text   ""
                  :disassembly "%1"
                  :size        1}

   :indirect     {:help-text   ""
                  :disassembly "%1 (%2)"
                  :size        3}

   :indirect-x   {:help-text   ""
                  :disassembly "%1 (%2,X)"
                  :size        2}

   :indirect-y   {:help-text   ""
                  :disassembly "%1 (%2X),Y"
                  :size        2}

   :zero-page    {:help-text   ""
                  :disassembly "%1 %2"
                  :size        2}

   :zero-page-x  {:help-text   ""
                  :disassembly "%1 %2,X"
                  :size        2}

   :unknown      {:help-text ""
                  :disassembly "%1 (UNKNOWN)"
                  :size 1 }

   :branch       {:help-text ""
                  :disassembly "%1 %2"
                  :size 2 }
   })


(def all-opcodes
  [{:opcode :BPL
    :mnemonic  "BPL"
    :help-text "Branch if xxxx"
    :flags     "none"
    :addressing-modes {0x10 :branch}}

   {:opcode :BMI
    :mnemonic  "BMI"
    :help-text "Branch if xxxx"
    :flags     "none"
    :addressing-modes {0x30 :branch}}

   {:opcode :BNE
    :mnemonic  "BNE"
    :help-text "Branch if xxxx"
    :flags     "none"
    :addressing-modes {0xd0 :branch}}

   {:opcode :BEQ
    :mnemonic  "BEQ"
    :help-text "Branch if xxxx"
    :flags     "none"
    :addressing-modes {0xf0 :branch}}

   {:opcode :BCC
    :mnemonic  "BCC"
    :help-text "Branch if xxxx"
    :flags     "none"
    :addressing-modes {0x90 :branch}}

   {:opcode :BCS
    :mnemonic  "BCS"
    :help-text "Branch if xxxx"
    :flags     "none"
    :addressing-modes {0xb0 :branch}}

   {:opcode :BVC
    :mnemonic  "BVC"
    :help-text "Branch if xxxx"
    :flags     "none"
    :addressing-modes {0x50 :branch}}

   {:opcode :BVS
    :mnemonic  "BVS"
    :help-text "Branch if xxxx"
    :flags     "none"
    :addressing-modes {0x70 :branch}}


   {:opcode :ADC
    :mnemonic       "ADC"
    :help-text      "ADd with Carry"

    :flags          "S V Z C"

    :addressing-modes {0x69 :immediate    ;; size 2  2 cycles
                       0x65 :zero-page    ;; size 2  3 cycles
                       0x75 :zero-page-x  ;; size 2  4 cycles
                       0x6d :absolute     ;; size 3  4 cycles
                       0x7d :absolute-x   ;; size 3  4+ cycles
                       0x79 :absolute-y   ;; size 3  4+ cycles
                       0x61 :indirect-x   ;; size 2  6 cycles
                       0x71 :indirect-y   ;; size 2  5+ cycles
                       }}

   {:opcode :AND
    :mnemonic       "AND"
    :help-text      "bitwise AND with accumulator"

    :flags          "S Z"

    :addressing-modes {0x29 :immediate    ;; size 2  2 cycles
                       0x25 :zero-page    ;; size 2  3 cycles
                       0x35 :zero-page-x  ;; size 2  4 cycles
                       0x2d :absolute     ;; size 3  4 cycles
                       0x3d :absolute-x   ;; size 3  4+ cycles
                       0x39 :absolute-y   ;; size 3  4+ cycles
                       0x21 :indirect-x   ;; size 2  6 cycles
                       0x31 :indirect-y   ;; size 2  5+ cycles
                       }}

   {:opcode :ASL
    :mnemonic       "ASL"
    :help-text      "Arithmetic Shift Left"

    :flags          "S Z C"

    :addressing-modes {0x0a :accumulator  ;; size 1  2 cycles
                       0x06 :zero-page    ;; size 2  5 cycles
                       0x16 :zero-page-x  ;; size 2  6 cycles
                       0x0e :absolute     ;; size 3  6 cycles
                       0x1e :absolute-x   ;; size 3  7 cycles
                       }}

   {:opcode :BIT
    :mnemonic       "BIT"
    :help-text      "test BITs"

    :flags          "N V Z"

    :addressing-modes {0x24 :zero-page ;; size 2  3 cycles
                       0x2c :absolute  ;; size 3  4 cycles
                       }}

   {:opcode :BRK
    :mnemonic       "BRK"
    :help-text      "BReaK"

    :flags          "B"

    :addressing-modes {0x00 :implied  ;; size 1  7 cycles
                       }} 

   {:opcode :CMP
    :mnemonic       "CMP"
    :help-text      "CoMPare accumulator"

    :flags          "S Z C"

    :addressing-modes {0xc9 :immediate    ;; size 2  2 cycles
                       0xc5 :zero-page    ;; size 2  3 cycles
                       0xd5 :zero-page-x  ;; size 2  4 cycles
                       0xcd :absolute     ;; size 3  4 cycles
                       0xdd :absolute-x   ;; size 3  4+ cycles
                       0xd9 :absolute-y   ;; size 3  4+ cycles
                       0xc1 :indirect-x   ;; size 2  6 cycles
                       0xd1 :indirect-y   ;; size 2  5+ cycles
                       }}

   {:opcode :CPX
    :mnemonic       "CPX"
    :help-text      "ComPare X register"

    :flags          "S Z C"

    :addressing-modes {0xe0 :immediate  ;; size 2  2 cycles
                       0xe4 :zero-page  ;; size 2  3 cycles
                       0xec :absolute   ;; size 3  4 cycles
                       }}


   {:opcode :INC
    :mnemonic       "INC"
    :help-text      "INCrement memory"

    :flags          "S Z"

    :addressing-modes {0xe6 :zero-page    ;; size 2  5 cycles
                       0xf6 :zero-page-x  ;; size 2  6 cycles
                       0xee :absolute     ;; size 3  6 cycles
                       0xfe :absolute-x   ;; size 3  7 cycles
                       }}
   {:opcode :INX
    :mnemonic       "INX"
    :help-text      "INCrement X register"

    :flags          "S Z"

    :addressing-modes {0xe8 :x-register   ;; size 1  2 cycles
                       }}
   {:opcode :DEX
    :mnemonic       "DEX"
    :help-text      "DEcrement X register"

    :flags          "S Z"

    :addressing-modes {0xca :x-register   ;; size 1  2 cycles
                       }}

   {:opcode :INY
    :mnemonic       "INY"
    :help-text      "INCrement Y register"

    :flags          "S Z"

    :addressing-modes {0xc8 :y-register   ;; size 1  2 cycles
                       }}

   {:opcode :DEY
    :mnemonic       "DEY"
    :help-text      "DEcrement Y register"

    :flags          "S Z"

    :addressing-modes {0x88 :y-register   ;; size 1  2 cycles
                       }}

   {:opcode :EOR
    :mnemonic       "EOR"
    :help-text      "bitwise Exclusive OR"

    :flags          "S Z"

    :addressing-modes {0x49 :immediate    ;; size 2  2 cycles
                       0x45 :zero-page    ;; size 2  3 cycles
                       0x55 :zero-page-x  ;; size 2  4 cycles
                       0x4d :absolute     ;; size 3  4 cycles
                       0x5d :absolute-x   ;; size 3  4+ cycles
                       0x59 :absolute-y   ;; size 3  4+ cycles
                       0x41 :indirect-x   ;; size 2  6 cycles
                       0x51 :indirect-y   ;; size 2  5+ cycles
                       }}
   {:opcode :JMP
    :mnemonic       "JMP"
    :help-text      "JuMP"

    :flags          "none"

    :addressing-modes {0x4c :absolute  ;; size 3  3 cycles
                       0x6c :indirect  ;; size 3  5 cycles
                       }}

   {:opcode :JSR
    :mnemonic       "JSR"
    :help-text      "Jump to SubRoutine"

    :flags          "none"

    :addressing-modes {0x20 :absolute  ;; size 3  6 cycles
                       }} 

   {:opcode :LDA
    :mnemonic       "LDA"
    :help-text      "LoaD Accumulator"

    :flags          "S Z"

    :addressing-modes {0xa9 :immediate    ;; size 2  2 cycles
                       0xa5 :zero-page    ;; size 2  3 cycles
                       0xb5 :zero-page-x  ;; size 2  4 cycles
                       0xad :absolute     ;; size 3  4 cycles
                       0xbd :absolute-x   ;; size 3  4+ cycles
                       0xb9 :absolute-y   ;; size 3  4+ cycles
                       0xa1 :indirect-x   ;; size 2  6 cycles
                       0xb1 :indirect-y   ;; size 2  5+ cycles
                       }}

   {:opcode :LDX
    :mnemonic       "LDX"
    :help-text      "LoaD X register"

    :flags          "S Z"

    :addressing-modes {0xa2 :immediate    ;; size 2  2 cycles
                       0xa6 :zero-page    ;; size 2  3 cycles
                       0xb6 :zero-page-y  ;; size 2  4 cycles
                       0xae :absolute     ;; size 3  4 cycles
                       0xbe :absolute-y   ;; size 3  4+ cycles
                       }}

   {:opcode :LDY
    :mnemonic       "LDY"
    :help-text      "LoaD Y register"

    :flags          "S Z"

    :addressing-modes {0xa0 :immediate    ;; size 2  2 cycles
                       0xa4 :zero-page    ;; size 2  3 cycles
                       0xb4 :zero-page-x  ;; size 2  4 cycles
                       0xac :absolute     ;; size 3  4 cycles
                       0xbc :absolute-x   ;; size 3  4+ cycles
                       }}

   {:opcode :LSR
    :mnemonic       "LSR"
    :help-text      "Logical Shift Right"

    :flags          "S Z C"

    :addressing-modes {0x4a :accumulator  ;; size 1  2 cycles
                       0x46 :zero-page    ;; size 2  5 cycles
                       0x56 :zero-page-x  ;; size 2  6 cycles
                       0x4e :absolute     ;; size 3  6 cycles
                       0x5e :absolute-x   ;; size 3  7 cycles
                       }}

   {:opcode :NOP
    :mnemonic       "NOP"
    :help-text      "No OPeration"

    :flags          "none"

    :addressing-modes {0xea :implied  ;; size 1  2 cycles
                       }} 

   {:opcode :ORA
    :mnemonic       "ORA"
    :help-text      "bitwise OR with Accumulator"

    :flags          "S Z"

    :addressing-modes {0x09 :immediate    ;; size 2  2 cycles
                       0x05 :zero-page    ;; size 2  3 cycles
                       0x15 :zero-page-x  ;; size 2  4 cycles
                       0x0d :absolute     ;; size 3  4 cycles
                       0x1d :absolute-x   ;; size 3  4+ cycles
                       0x19 :absolute-y   ;; size 3  4+ cycles
                       0x01 :indirect-x   ;; size 2  6 cycles
                       0x11 :indirect-y   ;; size 2  5+ cycles
                       }}

   {:opcode :ROL
    :mnemonic       "ROL"
    :help-text      "ROtate Left"

    :flags          "S Z C"

    :addressing-modes {0x2a :accumulator  ;; size 1  2 cycles
                       0x26 :zero-page    ;; size 2  5 cycles
                       0x36 :zero-page-x  ;; size 2  6 cycles
                       0x2e :absolute     ;; size 3  6 cycles
                       0x3e :absolute-x   ;; size 3  7 cycles
                       }}

   {:opcode :ROR
    :mnemonic       "ROR"
    :help-text      "ROtate Right"

    :flags          "S Z C"

    :addressing-modes {0x6a :accumulator  ;; size 1  2 cycles
                       0x66 :zero-page    ;; size 2  5 cycles
                       0x76 :zero-page-x  ;; size 2  6 cycles
                       0x6e :absolute     ;; size 3  6 cycles
                       0x7e :absolute-x   ;; size 3  7 cycles
                       }}

   {:opcode :RTI
    :mnemonic       "RTI"
    :help-text      "ReTurn from Interrupt"

    :flags          "all"

    :addressing-modes {0x40 :implied  ;; size 1  6 cycles
                       }} 

   {:opcode :RTS
    :mnemonic       "RTS"
    :help-text      "ReTurn from Subroutine"

    :flags          "none"

    :addressing-modes {0x60 :implied  ;; size 1  6 cycles
                       }}

   {:opcode :SBC
    :mnemonic       "SBC"
    :help-text      "SuBtract with Carry"

    :flags          "S V Z C"

    :addressing-modes {0xe9 :immediate    ;; size 2  2 cycles
                       0xe5 :zero-page    ;; size 2  3 cycles
                       0xf5 :zero-page-x  ;; size 2  4 cycles
                       0xed :absolute     ;; size 3  4 cycles
                       0xfd :absolute-x   ;; size 3  4+ cycles
                       0xf9 :absolute-y   ;; size 3  4+ cycles
                       0xe1 :indirect-x   ;; size 2  6 cycles
                       0xf1 :indirect-y   ;; size 2  5+ cycles
                       }}

   {:opcode :STA
    :mnemonic       "STA"
    :help-text      "STore Accumulator"

    :flags          "none"

    :addressing-modes {0x85 :zero-page    ;; size 2  3 cycles
                       0x95 :zero-page-x  ;; size 2  4 cycles
                       0x8d :absolute     ;; size 3  4 cycles
                       0x9d :absolute-x   ;; size 3  5 cycles
                       0x99 :absolute-y   ;; size 3  5 cycles
                       0x81 :indirect-x   ;; size 2  6 cycles
                       0x91 :indirect-y   ;; size 2  6 cycles
                       }}

   {:opcode :STX
    :mnemonic       "STX"
    :help-text      "STore X register"

    :flags          "none"

    :addressing-modes {0x86 :zero-page    ;; size 2  3 cycles
                       0x96 :zero-page-y  ;; size 2  4 cycles
                       0x8e :absolute     ;; size 3  4 cycles
                       }}

   {:opcode :STY
    :mnemonic       "STY"
    :help-text      "STore Y register"

    :flags          "none"

    :addressing-modes {0x84 :zero-page    ;; size 2  3 cycles
                       0x94 :zero-page-x  ;; size 2  4 cycles
                       0x8c :absolute     ;; size 3  4 cycles
                       }}
   {:opcode :CPY
    :mnemonic       "CPY"
    :help-text      "ComPare Y register"

    :flags          "S Z C"

    :addressing-modes {0xc0 :immediate  ;; size 2  2 cycles
                       0xc4 :zero-page  ;; size 2  3 cycles
                       0xcc :absolute   ;; size 3  4 cycles
                       }}


   ] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic opcode helper functions
(defn- branch-if [m func yes no]
  (if (func)
    (set-pc m yes)
    (set-pc m no)))

(defn- branch-if-not [m func yes no]
  (branch-if m #(not (func)) yes no))

(defn- do-next [m {:keys [next-instruction]}]
  (set-pc m next-instruction))

(defn- push-stack-word [m push-val]
  (assert false))

(defn- pop-stack-word [m]
  (assert false))

(defn- pop-stack-word [m pop-val]
  (assert false))

(defn- pop-stack-word [m]
  (assert false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def opcode-implementations
  {:UNKNOWN (fn [m opcode-record]
              (assert false))

   :ADC (fn [m opcode-record]
          (do-next m opcode-record))

   :AND (fn [m opcode-record]
          (do-next m opcode-record) )

   :ASL (fn [m opcode-record]
          (do-next m opcode-record) )

   :BIT (fn [m opcode-record]
          (do-next m opcode-record) )

   :CMP (fn [m opcode-record]
          (do-next m opcode-record) )

   :CPX (fn [m opcode-record]
          (do-next m opcode-record) )

   :INC (fn [m opcode-record]
          (do-next m opcode-record) )

   :INX (fn [m opcode-record]
          (do-next m opcode-record) )

   :DEX (fn [m opcode-record]
          (do-next m opcode-record) )

   :INY (fn [m opcode-record]
          (do-next m opcode-record) )

   :DEY (fn [m opcode-record]
          (do-next m opcode-record) )

   :EOR (fn [m opcode-record]
          (do-next m opcode-record) )

   :LDA (fn [m opcode-record]
          (do-next m opcode-record) )

   :LDX (fn [m opcode-record]
          (do-next m opcode-record) )

   :LDY (fn [m opcode-record]
          (do-next m opcode-record) )

   :LSR (fn [m opcode-record]
          (do-next m opcode-record) )

   :NOP (fn [m opcode-record]
          (do-next m opcode-record) )

   :ORA (fn [m opcode-record]
          (do-next m opcode-record) )

   :ROL (fn [m opcode-record]
          (do-next m opcode-record) )

   :ROR (fn [m opcode-record]
          (do-next m opcode-record) )

   :SBC (fn [m opcode-record]
          (do-next m opcode-record) )

   :STA (fn [m opcode-record]
          (do-next m opcode-record) )

   :STX (fn [m opcode-record]
          (do-next m opcode-record) )

   :STY (fn [m opcode-record]
          (do-next m opcode-record) )

   :CPY (fn [m opcode-record]
          (do-next m opcode-record) )

   ;; Done :)
   :BPL (fn [ m opcode-record ]
          (branch-if-not m #(get-n (:cpu m))))

   :BMI (fn [ m opcode-record ]
          (branch-if m #(get-n (:cpu m))))

   :BNE (fn [ m opcode-record ]
          (branch-if-not m #(get-z (:cpu m))))

   :BEQ (fn [ m opcode-record ]
          (branch-if m #(get-z (:cpu m))))

   :BCC (fn [ m opcode-record ]
          (branch-if-not m #(get-c (:cpu m))))

   :BCS (fn [ m opcode-record ]
          (branch-if m #(get-c (:cpu m))))

   :BVC (fn [ m opcode-record ]
          (branch-if-not m #(get-v (:cpu m))))

   :BVS (fn [ m opcode-record ]
          (branch-if m #(get-v (:cpu m))))

   :JMP (fn [m opcode-record]
          (set-pc m (:operand opcode-record)))

   :JSR (fn [m opcode-record]
          (push-stack-word (:next-instruction opcode-record))
          (set-pc m (:operand opcode-record)))

   :RTS  (fn [m _]
           (set-pc m (pop-stack-word m)))

   ;; TODO

   :BRK (fn [m opcode-record]
          (assert false)
          m)

   :RTI (fn [m opcode-record]
          (assert false)
          m )

   })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper utils for implementation of addressing modes
(defn- next-ins [addr-mode addr]
  (->> (addr-mode addressing-modes)
       (:size)  (fn [m opcode-ret]
                  m)
       (+ addr)))

(defn- ret-with-val [m addr-mode addr]
  {:addr addr
   :val (read-byte m addr)
   :next-instruction (next-ins addr-mode addr) })

(defn- byte-operand [m addr]
  (read-byte m (inc addr)))

(defn- word-operand [m addr]
  (read-word m (inc addr)))

(defn- ret-with-reg-val [m addr addr-mode reg]
  {:val              (-> m :cpu reg)
   :next-instruction (next-ins addr-mode addr)}  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Addressing mode implementations

(def addr-mode-implementations
  {:unknown      (fn [_ ^Integer addr]
                   {:next-instruction (next-ins :unknown addr)})

   :accumulator  (fn [m ^Integer addr]
                   (ret-with-reg-val m addr :accumulator :A))

   :x-register   (fn [m ^Integer addr]
                   (ret-with-reg-val m addr :x-register :X)) 

   :y-register   (fn [m ^Integer addr]
                   (ret-with-reg-val m addr :y-register :Y))

   :implied      (fn [m addr]
                   { :next-instruction (next-ins :implied addr)})

   :absolute     (fn [m ^Integer addr]
                   (->> (word-operand m addr)
                        (ret-with-val :absolute m)))

   :absolute-y   (fn [m ^Integer addr]
                   (->> (word-operand m addr)
                        (+ (-> m :cpu :Y))
                        (ret-with-val :absolute-y m))) 

   :absolute-x   (fn [m ^Integer addr]
                   (->> (word-operand m addr) 
                        (+ (-> m :cpu :X))
                        (ret-with-val :absolute-x m)))

   :immediate    (fn [m ^Integer addr]
                   (->> (inc addr)
                        (ret-with-val :immediate m)))

   :indirect     (fn [m ^Integer addr]
                   (->> (word-operand m addr)
                        (read-word m)
                        (ret-with-val :indirect m)))

   :indirect-x   (fn [m ^Integer addr]
                   (->> (byte-operand m addr)
                        (+ (-> m :cpu :X))
                        (read-word m) 
                        (ret-with-val :indirect-x m)))

   :indirect-y   (fn [m ^Integer addr]
                   (->> (byte-operand m addr)
                        (read-word m) 
                        (+ (-> m :cpu :Y))
                        (ret-with-val :indirect-y m)))

   :zero-page    (fn [m ^Integer addr]
                   (->> (byte-operand m addr)
                        (ret-with-val :zero-page m)))

   :zero-page-x  (fn [m ^Integer addr]
                   (->> (byte-operand m addr)
                        (+ (-> :cpu :X))
                        (ret-with-val :zero-page-x m)))

   ;; TODO Need to make sure address is a short
   ;;      probably best to do in ret-with-val

   :branch        (fn [m ^Integer addr]
                    (let [bval (byte-operand m addr)
                          delta (if (> bval 127)
                                  (- 0 (- bval 127))
                                  bval)]
                      (ret-with-val m :branch (+ addr delta)) ))})

(def unknown-opcode 
  {:opcode :UNKNOWN
   :mnemonic       "???"
   :help-text      "UKNONWN opocde"
   :flags          "none" })

(defn- mk-opcode-func [addr-mode-id opocde-id]
  (let [addr-mode-func (addr-mode-id addr-mode-implementations)
        opcode-func (opocde-id opcode-implementations) ]
    (fn [m]
      (println "got here!")
      (->> (get-pc m)
           (addr-mode-func m)
           (opcode-func m)))))

(defn- mk-opcode-entry [opcode addressing-mode addressing-modes]
  (merge 
    {:addressing-mode addressing-mode
     :func (mk-opcode-func addressing-mode (:opcode opcode) )}
    (dissoc opcode :addressing-modes)))

;; Construct Opcode -> opcode table
(defn- mk-opcode-table
  "Take the opcodes and make a table indexed by opcode hex"
 [opcode addressing-modes tab]
  (let [addr-modes (map identity (:addressing-modes opcode))
        as-map (map identity addr-modes)
        my-fn ( fn [tab [hex addressing-mode]]
                    (assoc
                      tab
                      hex (mk-opcode-entry opcode addressing-mode addressing-modes))) ]
    (reduce my-fn tab as-map)))


(defn- mk-big-opcode-table [all-opcodes addressing-modes]
  (let [default-tab  (->> (mk-opcode-entry unknown-opcode :unknown addressing-modes)
                          (mk-vec 256)
                          )]
    (reduce (fn [t opcode]
              (mk-opcode-table opcode addressing-modes t) ) default-tab all-opcodes)))

(def opcode-table
  (mk-big-opcode-table all-opcodes addressing-modes))

(defn get-opcode [opcode-hex] 
  (get opcode-table opcode-hex unknown-opcode))



