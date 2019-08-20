(ns advent-of-code.day20-17-pt1
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def test-particles
  "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>")

(def particle-pattern #".=<(-?\d+),(-?\d+),(-?\d+)>")

(defn parse-particle [p]
  "Parses particle string and returns coordinate values for p,v,a: [[x y z] [x y z] [x y z]]"
  (let [[_ x y z] (re-matches particle-pattern p)]
    (mapv #(Integer/parseInt %) [x y z])))

(defn parse-particles [particle-input]
  (map #(mapv parse-particle %) (map #(s/split % #", ") (s/split-lines particle-input))))

(def pos 0)

(def vel 1)

(def acc 2)

(def xc 0)

(def yc 1)

(def zc 2)

(def base [[0 0 0] [0 0 0] [0 0 0]])

(defn increase [p ks1 ks2]
  (update-in p ks1 #(+ % (get-in p ks2))))

(defn update-particle [p]
  (-> p
      (increase [vel xc] [acc xc])
      (increase [vel yc] [acc yc])
      (increase [vel zc] [acc zc])
      (increase [pos xc] [vel xc])
      (increase [pos yc] [vel yc])
      (increase [pos zc] [vel zc])))

(defn particles-abs-diff [p1 p2]
  (reduce + (map #(Math/abs (- %1 %2)) (p1 pos) (p2 pos))))

(defn particle-base-dist [p]
  (reduce + (map #(Math/abs %) (p pos))))

(defn particle-abs-single-move [p]
  (particles-abs-diff p (update-particle p)))

(defn particle-abs-move-growth [p]
  (Math/abs
   (- (particle-abs-single-move p) (particle-abs-single-move (update-particle p)))))

(defn compare-particles [p1 p2]
  (cond
    (< (particle-abs-move-growth p1)(particle-abs-move-growth p2))  -1
    (< (particle-abs-move-growth p2)(particle-abs-move-growth p1))   1
    (< (particle-abs-single-move p1) (particle-abs-single-move p2)) -1
    (< (particle-abs-single-move p2) (particle-abs-single-move p1))  1
    :else
    (compare (particle-base-dist p1) (particle-base-dist p2))))

(def test-input
  (vec (parse-particles test-particles)))

(def input
  (vec (parse-particles (slurp (io/resource "day20-17.txt")))))

(defn particle-swarm [input]
  (let [p (first (sort-by identity compare-particles input))]
    (.indexOf input p)))

;;; POSITION!!!! AHHHHHH
;;; HMMM...Didn't work. Maybe just come back some time and try a simple brute force simulation.
