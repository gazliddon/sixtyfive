(ns sixtyfive.utils
  (:require [clojure.reflect :refer :all]
            [clojure.pprint :refer :all]))

(defn refl [a] (->> (reflect a)
                    (pprint)))
