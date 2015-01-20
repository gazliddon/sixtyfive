(ns sixtyfive.protocols)

(defprotocol IMemoryReader
  (read-word  [_ ^long addr])
  (read-byte  [_ addr])
  (read-block [_ ^long src ^long size]))

(defprotocol IMemoryWriter
  (write-block [_ ^long dst src])
  (write-byte  [_ ^long ^long addr v])
  (write-word  [_ ^long ^long addr v]))

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



