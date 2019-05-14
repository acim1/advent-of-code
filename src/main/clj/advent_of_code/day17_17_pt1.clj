(ns advent-of-code.day17-17-pt1
  (:import (advent_of_code.java LinkedListNode)
           (java.util Date)))

(defn step-insert [[step val node]]
  (loop [step step node node next-node (.getNext node)]
    (if (<= step 0)
      (do
        (when (= (mod val 1000000) 0)
          (println (str "Reached: " val " at " (Date.))))
        (.setNext node (LinkedListNode. val next-node))
        [step (inc val) (.getNext node)])
      (recur (dec step) (.getNext node) (.. node (getNext) (getNext))))))

(defn spinlock [step]
  (let [root (LinkedListNode. 0 nil)]
    (do
      (.setNext root root) ;; circular
      (loop [val 1 node root]
        (if (> val 50000000)
          (.. root (getNext) (getVal))
          (let [[_ new-val cur-node] (step-insert [step val node])]
            (recur new-val cur-node)))))))
