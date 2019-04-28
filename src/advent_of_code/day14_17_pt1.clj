(ns advent-of-code.day14-17-pt1
  (:require [advent-of-code.day10-17-pt2 :as d10]))

(def test-input
  "flqrgnkx")

(def input
  "amgozmfv")

(defn ->kh-input [input]
  (let [suffix [17,31,73,47,23]]
    [(range 0 256)
     (concat (map int input) suffix)]))

(defn kh->num [hash]
  (.bitCount (BigInteger. hash 16)))

(defn defrag [input]
  (reduce + (for [n (range 0 128)]
              (-> (str input "-" n)
                  ->kh-input
                  d10/knot-hash2
                  kh->num))))
