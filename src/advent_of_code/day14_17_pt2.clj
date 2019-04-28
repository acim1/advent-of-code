(ns advent-of-code.day14-17-pt2
  (:require [advent-of-code.day10-17-pt2 :as d10]))

(def test-input
  "flqrgnkx")

(def input
  "amgozmfv")

(defn ->kh-input [input]
  (let [suffix [17,31,73,47,23]]
    [(range 0 256)
     (concat (map int input) suffix)]))

(defn kh->bs [hash]
  (-> (BigInteger. hash 16)
      (.toString 2)
      ((fn [s] (map #(= \1 %) s)))
      boolean-array))

(defn defrag [input]
  (let [bs (make-array Boolean/TYPE 128 0)]
    (do
      (doseq [n (range 0 128)]
        (->> (str input "-" n)
             ->kh-input
             d10/knot-hash2
             kh->bs
             (aset bs n)))
      bs)))
