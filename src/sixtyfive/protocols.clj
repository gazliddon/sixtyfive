(ns sixtyfive.protocols)

(defprotocol IMemoryReader
  (read-word [_ ^long addr])
  (read-byte [_ ^long addr])
  (read-block [_ ^long src ^long size]))

(defprotocol IMemoryWriter
  (write-block [_ ^long dst src])
  (write-byte [_ ^long addr ^long v])
  (write-word [_ ^long addr ^long v]))

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

