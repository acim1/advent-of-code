(ns advent-of-code.day19-17-pt1
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def test-input 
  "     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ ")

(defn input []
  (-> (io/resource "day19-17.txt") slurp))

(defn advance [[row col] dir]
  (case dir
    :u [(dec row) col]
    :d [(inc row) col]
    :l [row (dec col)]
    :r [row (inc col)]))

(defn opposite-dir [dir]
  (case dir
    :u :d
    :d :u
    :l :r
    :r :l))

(defn position? [lines [row col]]
  (some-> lines
          (nth row nil)
          (nth col nil)
          (not= \ )))

(defn next-dir [lines pos dir]
  (let [opp (opposite-dir dir)
        nxt (some #(when (position? lines (advance pos %)) %)
                  (remove #(= % opp) [dir :u :d :l :r]))]
    nxt))

(defn series-of-tubes [input]
  (let [lines     (s/split-lines input)
        start-col (s/index-of (first lines) \|)]
    (loop [[row col :as pos] [0 start-col] dir :d letters ""]
      (if-not (position? lines pos)
        letters
        (let [pos-char (-> lines
                           (nth row)
                           (nth col))]
          (case pos-char
            (\| \-) (recur (advance pos dir) dir letters)
            \+      (let [nxt (next-dir lines pos dir)]
                      (recur (advance pos nxt) nxt letters))
            (recur (advance pos dir) dir (str letters pos-char))))))))

