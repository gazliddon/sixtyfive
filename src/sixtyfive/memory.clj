(ns sixtyfive.memory
  (:require [sixtyfive.protocols :refer :all])
  )

(defrecord ByteMemory [data]

  IMemoryReader
  (read-block [_ src size]
    (->> (drop src data)
         (take size)
         (vec)))

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
