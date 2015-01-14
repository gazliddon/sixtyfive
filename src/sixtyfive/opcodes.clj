(ns sixtyfive.opcodes
  (:require [sixtyfive.cpu :refer :all])
  (:import [sixtyfive.cpu Cpu]
           )
  (:gen-class))

(defprotocol IOpCodeFactory
  (get-name [_])
  (get-addr-modes [_])
  (make-func [_ addr-mode]) )

(defprotocol IOpCode
  (exec [_ ^Machine mac])
  (get-size [_])
  (get-cycles [_]))

(defn mk-opcode [opcode addr-mode]
  (let [func [make-func opcode addr-mode]]
    (reify IOpCode

      (exec [_ mac]
        (func mac))

      (get-size [_]
        1)

      (get-cycles [_]
        1))))

(def unsupported-op-code

  (reify IOpCode
      (exec [_ mac]
        (assert false)
        )

      (get-size [_]
        99)

      (get-cycles [_]
        99))
  )

(defn get-opcode-implementations
  "generates a vector with info on differnt addr modes for this op
  [ [hex-num <opcode implementation>]"
  [opcode id-to-addr-mode]

  (let [addr-modes (get-addr-modes opcode)
        t-func (fn [v]
                 (let [addr-mode (id-to-addr-mode (addr-modes v)) ]
                   [v (mk-opcode opcode addr-mode)]))
        t-imps (mapv t-func (keys addr-modes)) ]
    t-imps
    ))

(defn add-opcode-functions [opcode-tab opcode id-to-addr-mode]
  (let [tups (get-opcode-implementations opcode id-to-addr-mode)]
    (reduce (fn [t [hx op-code-imp]] (assoc t hx op-code-imp)) opcode-tab tups)))

(defn make-op-code-tab [opcode-factories id-to-addr-mode]
  (let [ret-tab (vec  (take 256 (repeat unsupported-op-code))) ]
    (reduce (fn [t v] (add-opcode-functions t v id-to-addr-mode)) ret-tab opcode-factories)))
