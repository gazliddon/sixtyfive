(ns sixtyfive.prg
  (:require [sixtyfive.protocols :refer :all]
            [clojure.java.io :refer [file output-stream input-stream]]
            ))

(defrecord Prg [^Integer address data])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File helpy rubbish
;; TODO Should be in seperate class and
;;      Should be clj / cljs agnostic

(defn- file-length [^String file-name]
  (let [f (file file-name)]
    (.length f)))

(defn- load-bin [^String file-name]
  (with-open [in (input-stream (file file-name))]
    (let [buf (byte-array (file-length file-name))
          _ (.read in buf) ]
      buf)))

(defn- bb-as-vec [byte-buffer]
  (let [conv #(if (< %1 0) (+ 256 %1)  %1)]
    (mapv conv (into [] byte-buffer))))

(defn- load-bin-as-vec [^String file-name]
  (bb-as-vec (load-bin file-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn load-prg [machine ^Prg {:keys [address data]}]
  (-> machine
    (write-block address data)
    (set-pc address)))

(defn make-prg-from-file ^Prg [^String file-name]
  (let [as-vec (load-bin-as-vec  file-name)
        load-l (nth as-vec 0)
        load-h (nth as-vec 1) ]
    (->Prg (+ load-l (* 256 load-h)) (vec (drop 2 as-vec)))))

(defn load-prg-from-file [machine file-name]
  (->> (make-prg-from-file file-name)
       (load-prg machine)))


