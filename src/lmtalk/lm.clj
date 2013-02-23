(ns lmtalk.lm
  (:require [clojure.string :as str]))


(defn n-gram [sent n]
  "generate n gram from a sentence
  sent is a sequence of words"
  (let [spad (repeat (dec n) :start)
        epad '(:end)]
    (partition n 1 (concat spad sent epad))))
