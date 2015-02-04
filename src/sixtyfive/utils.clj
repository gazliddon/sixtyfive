(ns sixtyfive.utils
  (:require [clojure.reflect :refer :all]
            [clojure.pprint  :refer :all]))

(defn refl [a] (->> (reflect a)
                    (pprint)))

(defn mk-vec
  "make a vector of size filled with item"
  [size item]
  (->> (repeat item)
       (take 256)
       (vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell style flip
(defn flip [function] 
  (fn 
    ([] (function))
    ([x] (function x)) 
    ([x y] (function)) 
    ([x y z] (function z y x)) 
    ([a b c d] (function d c b a)) 
    ([a b c d & rest]
        (->> rest
            (concat [a b c d])
            reverse
            (apply function)))))
