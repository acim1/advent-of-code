(ns advent-of-code.day13-17-pt2
  (:require [clojure.java.io :as io]))

(def test-input
  "0: 3\n1: 2\n4: 4\n6: 4")

(defn get-input
  ([] (-> (io/resource "day13-17.txt")
          slurp
          get-input))
  ([s] (->> s
            clojure.string/split-lines
            (map #(clojure.string/split % #": "))
            (map (fn [coll] (let [[layer rng] (vec (map #(Integer/parseInt %) coll))]
                              [layer [0 rng]])))
            (into (sorted-map)) 
            (#(let [end (-> % last first)
                    default-map (into (sorted-map) 
                                      (zipmap (range (inc end))(repeat [-1 0])))]
                (merge default-map %))))))

(defn adv-position-range [[position rng]]
  (let [next-pos (if (pos? rng) (inc position) (dec position))]
    (cond
      ;; switch to going up
      (neg? next-pos)  [1 (- rng)]
      ;; switch to going down
      (= next-pos rng) [(dec position) (- rng)]
      ;; proceed in current direction
      :else [next-pos rng])))

(defn advance-scanners [scanners]
  (reduce-kv (fn [m layer [position rng :as pos-rng]]
               (if (zero? rng)
                 (assoc m layer pos-rng) ; no scanner 
                 (assoc m layer (adv-position-range pos-rng)))) 
             (sorted-map)
             scanners))

(defn caught? [layer scanners]
  (let [[position rng] (scanners layer)]
    (zero? position)))

(defn packet-scanners-clean? [scanners]
  (let [final-layer (-> scanners last first)]
    (loop [layer 0 scanners scanners]
      (cond
        (> layer final-layer)    true
        (caught? layer scanners) false 
        :else (recur (inc layer) (advance-scanners scanners))))))

(defn delay-packet-scanners [scanners]
  (loop [delay 0 scanners scanners]
    (if (packet-scanners-clean? scanners)
      delay
      (recur (inc delay) (advance-scanners scanners)))))
