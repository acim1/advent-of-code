(ns advent-of-code.day21-17-pt1
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def seed-pattern
  ".#./..#/###")

(defn ->vec [pattern]
  (mapv vec (s/split pattern #"\/")))

(defn parse-rules [raw]
  (into {} (for [line (s/split-lines raw)
                 :let [[pattern rule] (s/split line #" => ")]]
             [(->vec pattern) (->vec rule)])))

(defn rules []
  (parse-rules (slurp (io/resource "day21-17.txt"))))

(def test-rules
  (parse-rules "../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#"))

(defn size [p]
  (count (first p)))

(defn transpose [m]
  (apply mapv vector m))

(defn flip [p]
  (mapv (comp vec reverse) p))

(defn rotate-clockwise [p]
  ((comp flip transpose) p))


(defn match? [p r]
  (some #(= r %) (concat
                  (take 4 (iterate rotate-clockwise p))
                  (take 4 (iterate rotate-clockwise (flip p))))))

(defn next-block [x y p block-size]
  (loop [i 0 block []]
    (if (>= i block-size)
      block
      (recur (inc i) (conj block (subvec (nth p (+ y i)) x block-size))))))

(defn blocks [p block-size]
  (loop [x 0 y 0 bs []]
    (cond
      (< x (size p))
      (recur (+ x block-size) y (conj bs (next-block x y p block-size)))

      (< (+ y block-size) (size p))
      (recur block-size (+ y block-size) (conj bs (next-block 0 (+ y block-size) p block-size)))

      :else bs)))

(defn break [p]
  (let [n (if (zero? (mod (size p) 2)) 2 3)]
    (blocks n)))

(defn fractal-art [])
