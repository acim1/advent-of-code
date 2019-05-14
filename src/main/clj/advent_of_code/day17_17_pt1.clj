(ns advent-of-code.day17-17-pt1
  (:require [clojure.java.io :as io]))


(defn step-insert [[index step val buffer]]
  (let [insert-index (inc (mod (+ index step) (count buffer)))
        [xs ys] (split-at insert-index buffer)]
    [insert-index step (inc val) (concat xs (cons val ys))]))

(defn spinlock [step]
  (loop [index 0 val 1 buffer (list 0)]
    (if (> val 50000000)
      (nth buffer 1)
      (let [[new-index _ new-val new-buffer] (step-insert [index step val buffer])]
        (recur new-index new-val new-buffer)))))
