(ns advent-of-code.day21-17-pt1
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def seed-pattern
  ".#.
..#
###")

(defn parse-rules [raw])

(defn rules []
  (parse-rules (slurp (io/resource "day21-17.txt"))))

(defn transpose [m]
  (apply mapv vector m))

(defn flip [p]
  (mapv (comp vec reverse) p))

(defn rotate-clockwise [p]
  ((comp flip transpose) p))



(defn size [pattern]
  (count (first pattern)))

(defn fractal-art [])
