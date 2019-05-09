(ns advent-of-code.day16-17-pt1
  (:require [clojure.java.io :as io]))

(defn spin [programs n]
  (let [[front back] (split-at (- (count programs) n) (vals programs))]
    (into (sorted-map) (zipmap (range 0 (count programs)) (concat back front)))))

(defn exchange [programs i j]
  (-> programs
      (assoc i (programs j))
      (assoc j (programs i))))

(defn partner [programs a b]
  (let [programs-inverted (clojure.set/map-invert programs)]
    (-> programs
        (assoc (programs-inverted a) b)
        (assoc (programs-inverted b) a))))

(defn parse-instructions [instructions]
  (for [instr instructions]
    (let [[_ n :as sX]    (re-matches #"s(\d+)" instr)
          [_ i j :as xAB] (re-matches #"x(\d+)\/(\d+)" instr)
          [_ a b :as pAB] (re-matches #"p([a-z])\/([a-z])" instr )]
      (cond
        sX  #(spin % (Integer/parseInt n))
        xAB #(exchange % (Integer/parseInt i) (Integer/parseInt j))
        pAB #(partner % (first a) (first b))
        :else (throw (IllegalArgumentException. (str "No instruction match for: " instr)))))))

(def test-input [(into (sorted-map) (zipmap (range 0 16) "abcde"))
                 (parse-instructions (clojure.string/split "s1,x3/4,pe/b" #","))
                 {}])

(defn input []
  [(into (sorted-map) (zipmap (range 0 16) "abcdefghijklmnop"))
   (-> (io/resource "day16-17.txt")
       slurp
       (clojure.string/split #",")
       parse-instructions)
   {}])

(defn permutation-promenade [[orig-programs orig-instructions programs->next]]
  (if (programs->next orig-programs)
    [(programs->next orig-programs) orig-instructions programs->next] ;; we already have answer
    (loop [programs orig-programs [instr & instructions] orig-instructions]
      (if-not instr
        [programs orig-instructions (assoc programs->next orig-programs programs)]
        (recur (instr programs) instructions)))))


