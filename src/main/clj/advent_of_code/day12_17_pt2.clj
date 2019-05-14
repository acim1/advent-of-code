(ns advent-of-code.day12-17-pt2
  (:require [clojure.java.io :as io]))

(def input
  (slurp (io/resource
           "day12-17.txt")))

(def test-input
  "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5")

(defn build-map [input]
  (loop [[x & xs] (clojure.string/split-lines input) m {}]
    (if-not x
      m
      (let [[_ k vs] (re-matches #"(.+) <-> (.+)" x)
            k'  (Integer/parseInt k)
            vs' (into #{} (map #(Integer/parseInt %) (clojure.string/split vs #", ")))]
        (recur xs (assoc m k' vs'))))))

(defn digital-plumb [initial-map id]
  (loop [m initial-map ids #{id}]
    (if (or (empty? m) (empty? ids))
      m
      (let [next-ids (apply clojure.set/union (map m ids))
            next-m   (apply dissoc m ids)]
        (recur next-m next-ids)))))

(defn count-groups [input]
  (let [initial-map (build-map input)]
    (loop [m initial-map n 0]
      (if (empty? m)
        n
        (recur (digital-plumb m (first (keys m))) (inc n))))))


