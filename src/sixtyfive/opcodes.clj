(ns sixtyfive.opcodes
  (:require [sixtyfive.cpu :refer :all])
  (:import [sixtyfive.cpu Cpu])
  (:gen-class))

(defprotocol IOpCodeFactory
  (get-name [_])
  (get-addr-modes [_])
  (make-func [_ addr-mode]) )

(defn get-opcode-implementations [opcode id-to-addr-mode]
  (let [addr-modes (get-addr-modes opcode)
        t-func (fn [v]
                 [v (id-to-addr-mode (addr-modes v))])
        ]
    (mapv t-func (keys addr-modes))))

(defn add-opcode-functions [opcode-tab opcode id-to-addr-mode]
  (let [tups (get-opcode-implementations opcode id-to-addr-mode)]
    (reduce (fn [t [hx func]] (assoc t hx func)) opcode-tab tups)))

(defn make-op-code-tab [opcode-factories id-to-addr-mode]
  (let [ret-tab (vec  (take 256 (repeat 0))) ]
    (reduce (fn [t v] (add-opcode-functions t v id-to-addr-mode)) ret-tab opcode-factories)))
