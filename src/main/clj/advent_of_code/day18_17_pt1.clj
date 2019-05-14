(ns advent-of-code.day18-17-pt1
  (:require [clojure.java.io :as io]))

(def test-input
  (-> "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"
      (clojure.string/split-lines)))

(def input
  (-> "set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 735
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19"
      (clojure.string/split-lines)))

;; just starts with PC address; we'll add :snd and :rcv for last snd and rcv, and registers as we go
(def initial-vm {:pc 0}) 

(defn val [vm x]
  (if (re-matches #"[a-z]" x)
    (get vm x 0)
    (Long/parseLong x)))

(defn parse-instruction [i]
  (if-let [[_ op x y] (re-matches #"(snd|set|add|mul|mod|rcv|jgz) ([a-z]|-?\d+) ?([a-z]|-?\d+)?" i)]
    (case op
      "snd" #(assoc % :snd (val % x))
      "set" #(assoc % x (val % y))
      "add" #(update % x (fnil (partial + (val % y)) 0))
      "mul" #(update % x (fnil (partial * (val % y)) 0))
      "mod" #(update % x (fnil (fn [xv] (mod xv (val % y))) 0))
      "rcv" #(if-not (zero? (val % x)) (assoc % :rcv (:snd %)) %)
      "jgz" #(if (pos? (val % x)) (update % :pc (fnil (partial + (dec (val % y))) 0)) %))
    (throw (IllegalArgumentException. (format "Cannot parse instruction '%s'" i)))))

(defn duet [instructions]
  (let [xs (mapv parse-instruction instructions)]
    (loop [vm ((first xs) initial-vm)]
      (or (:rcv vm)
          (recur (-> vm
                     (update :pc inc)
                     (#(let [x (xs (:pc %))]
                         (x %)))))))))
