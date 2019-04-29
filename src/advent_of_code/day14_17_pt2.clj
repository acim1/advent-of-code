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
      ((fn [s] (concat (repeat (- 128 (count s)) \0) s))) ; pad left with zeroes
      ((fn [s] (map #(= \1 %) s)))
      boolean-array))

(defn remove-region [bs i j]
  (and
   ;; boundary checks, test
   (>= i 0)
   (>= j 0)
   (< i (count bs))
   (< j (count bs))
   (aget bs i j)
   ;; removal
   (do
     (aset-boolean bs i j false)
     (remove-region bs (dec i) j) ; up
     (remove-region bs (inc i) j) ; down
     (remove-region bs i (dec j)) ; left
     (remove-region bs i (inc j)) ; right
     true))) 

(defn defrag [input]
  (let [bs (make-array Boolean/TYPE 128 0)]
    (do
      (doseq [n (range 0 128)]
        (->> (str input "-" n)
             ->kh-input
             d10/knot-hash2
             kh->bs
             (aset bs n)))
      (reduce + (for [i (range 0 128)
                      j (range 0 128)]
                  (if (remove-region bs i j) 1 0))))))
