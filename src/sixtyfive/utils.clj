(ns sixtyfive.utils
  (:require [clojure.reflect :refer :all]
            [clojure.pprint :refer :all]))

(defn refl [a] (->> (reflect a)
                    (pprint)))


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
