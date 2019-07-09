(ns advent-of-code.day20-17-pt1
  (:require [clojure.string :as s]))

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
  (reduce + (map #(Math/abs (- %1 %2)) (flatten p1) (flatten p2))))

(defn particle-abs-move [p]
  (Math/abs
   (-
    (particles-abs-diff p (update-particle p))
    (particles-abs-diff (update-particle p) (update-particle (update-particle p))))))

(defn particle-abs-base-diff [p]
  (particles-abs-diff [[0 0 0] [0 0 0] [0 0 0]] p))

(defn compare-particles [p1 p2]
  (cond
    (< (particle-abs-move p1) (particle-abs-move p2)) -1
    (< (particle-abs-move p2) (particle-abs-move p1)) 1
    :else (compare (particle-abs-base-diff p1) (particle-abs-base-diff p2))))

(def test-input
  (vec (parse-particles test-particles)))

(defn particle-swarm [input]
  (let [p (first (sort-by identity compare-particles input))]
    (.indexOf input p)))
