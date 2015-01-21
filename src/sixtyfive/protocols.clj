(ns sixtyfive.protocols)

(defprotocol IMachine
  (get-pc [_ ])
  (set-pc [_ new-pc])
  (step [_]))

(defprotocol IMemoryReader
  (read-word  [_ addr])
  (read-byte  [_ addr])
  (read-block [_ src size]))

(defprotocol IMemoryWriter
  (write-block [_ dst src])
  (write-byte  [_ addr v])
  (write-word  [_ addr v]))

(defprotocol IAddrMode
  (get-str [_])
  (get-operand-size [_])
  (get-operand [_ m addr])
  (calculate-address [_ m ]))

(defprotocol IOpCodeFactory
  (get-name [_])
  (get-addr-modes [_])
  (make-func [_ addr-mode]) )

(defprotocol IOpCode
  (get-factory [_])
  (get-addr-mode [_])
  (exec-opcode [_ mac])
  (get-size [_])
  (get-cycles [_]))



