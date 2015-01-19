(ns sixtyfive.memory
  (:gen-class))

(defprotocol IMemoryReader
  (read-word [_ ^long addr])
  (read-byte [_ ^long addr])
  (read-block [_ ^long src ^long size]))

(defprotocol IMemoryWriter
  (write-block [_ ^long dst src])
  (write-byte [_ ^long addr ^long v])
  (write-word [_ ^long addr ^long v]))

(defrecord ByteMemory [data]

  IMemoryReader
  (read-block [_ src size]
    (vec (take size (drop src data))))

  (read-word [m addr]
    (let [l (.read-byte m addr)
          h (.read-byte m (inc addr)) ]
      (+ l (* 0x100 h))))

  (read-byte [_ addr]
    (bit-and 0xff (nth data addr)))


  IMemoryWriter
  (write-block [{:keys [data] :as m} dst src ]
    (let [slice-0 (take dst data)
          slice-1 (drop (+ dst (count src)) data )]
      (assoc m :data (vec (concat (vec slice-0) src (vec slice-1))))))
  (write-word [m addr v]
    (let [l (bit-and 0xff v)
          h (bit-and 0xff (/ v 0x100))]
      (.write-block m addr [l h])))

  (write-byte [this addr v]
    (assoc-in this [:data addr] (bit-and 0xff v) )))

(defn mk-byte-memory [size]
  (->ByteMemory
    (vec  (take size (repeat 0)))))
