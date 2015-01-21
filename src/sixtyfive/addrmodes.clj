(ns sixtyfive.addrmodes
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
