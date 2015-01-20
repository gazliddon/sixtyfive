(ns sixtyfive.cpu
  )

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn make-byte [v] (bit-and 0xff v))
(defn make-word [v] (bit-and 0xffff v))

(defn get-lo-hi [v]
  [(make-byte v)
   (make-byte (bit-shift-right v 8))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn overflowed? [old-val new-val]
  (bit-test (bit-xor old-val new-val) 7))

(defn is-neg? [val]
  (bit-test val 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:const C (bit-set 0 0))
(def ^:const Z (bit-set 0 1))
(def ^:const I (bit-set 0 2))
(def ^:const D (bit-set 0 3))
(def ^:const B (bit-set 0 4))
(def ^:const V (bit-set 0 5))
(def ^:const N (bit-set 0 6))

(defrecord Cpu
  [^long PC
   ^long A
   ^long X
   ^long Y
   ^long S
   ^long flags])

;; Make a cpu convinience func
(defn mk-cpu []
  (->Cpu 0 0 0 0 0xff 0))

(defn or-flags [^Cpu {:keys [flags] :as cpu} ^long v]
  (assoc cpu :flags (bit-or flags v)))

(defn mask-flags [^Cpu {:keys [flags] :as cpu} ^long v]
  (assoc cpu :flags (bit-and flags (bit-not v))))

(defn set-flag [^Cpu cpu flag-mask v]
  (if v
    (or-flags cpu flag-mask)
    (mask-flags cpu flag-mask)))

(defn get-flag [^Cpu {:keys [flags]} flag-mask]
  (not= 0 (bit-and flags flag-mask)))

;; Flag getter / setters
(defn set-c [^Cpu cpu v] (set-flag cpu C v))
(defn get-c [^Cpu cpu] (get-flag cpu C))

(defn set-z [^Cpu cpu v] (set-flag cpu Z v))
(defn get-z [^Cpu cpu] (get-flag cpu Z))

(defn set-v [^Cpu cpu v] (set-flag cpu V v))
(defn get-v [^Cpu cpu] (get-flag cpu V))

(defn set-n [^Cpu cpu v] (set-flag cpu N v))
(defn get-n [^Cpu cpu] (get-flag cpu N))

(defn set-d [^Cpu cpu v] (set-flag cpu D v))
(defn get-d [^Cpu cpu] (get-flag cpu D))

(defn get-operand-address [^Cpu cpu]
  (bit-and 0xffff  (inc  (:PC cpu))))

(defn set-pc [^Cpu cpu ^long addr]
  (assoc cpu :PC (make-word addr)))

(defn get-pc [^Cpu {:keys [PC]}]
  PC)

(defn set-reg [^Cpu cpu reg v]
  (assoc cpu reg (make-byte v)))

(defn get-reg [^Cpu cpu reg]
  (reg cpu))



