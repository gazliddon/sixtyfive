(ns sixtyfive.memory
  (:gen-class))

(defprotocol IMemory
  (write-block [_ ^long dst src])
  (read-block [_ ^long src ^long size])
  (read-byte [_ ^long addr])
  (write-byte [_ ^long addr ^long v]))

(defrecord ByteMemory [mem]
  IMemory
  (read-block [_ src size]
    (vec (take size (drop src))))

  (write-block [_ dst src ]
    (let [slice-0 (take dst mem)
          slice-1 (drop (+ dst (count src)) mem )]
      (vec  (concat (vec slice-0) src (vec slice-1) ))))

  (read-byte [_ addr]
    (bit-and 0xff (nth mem addr)))

  (write-byte [this addr v]
    (assoc-in this [:mem addr] (bit-and 0xff v) )))

(defn mk-byte-memory [size]
  (->ByteMemory
    (vec  (take size (repeat 0)))))
