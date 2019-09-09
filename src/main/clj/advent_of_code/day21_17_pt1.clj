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


(defn some-match [p rs]
  (rs (some (set (keys rs)) (concat
                             (take 4 (iterate rotate-clockwise p))
                             (take 4 (iterate rotate-clockwise (flip p)))))))

(defn next-square [x y p square-size]
  (loop [i 0 square []]
    (if (>= i square-size)
      square
      (recur (inc i) (conj square (subvec (nth p (+ y i)) x (+ x square-size)))))))

(defn ->squares [p square-size]
  (loop [x 0 y 0 squares []]
    (cond
      (< x (size p))
      (recur (+ x square-size) y (conj squares (next-square x y p square-size)))

      (< (+ y square-size) (size p))
      (recur square-size (+ y square-size) (conj squares (next-square 0 (+ y square-size) p square-size)))

      :else squares)))

(defn break [p]
  (let [n (if (zero? (mod (size p) 2)) 2 3)]
    (->squares p n)))


(defn concat-rows [squares]
  (mapv vec (apply map concat squares)))

(defn join-squares [squares]
  (let [n (int (Math/sqrt (count squares)))]
    (mapv concat-rows (partition n squares))))

(defn fractal-art [iterations rules]
  (loop [n iterations p (->vec seed-pattern)]
    (do
      (println p)
      (println n)
      (if (zero? n)
        (cons (count (filter #(= % \#) (flatten p))) p)
        (recur (dec n) (join-squares (mapv #(some-match % rules) (break p))))))))

