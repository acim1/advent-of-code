(ns advent-of-code.day18-17-pt1
  (:require [clojure.java.io :as io]))

(def test-input
  (-> "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d"
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
(def initial-prog {:pc 0 :status :run :snd-cnt 0}) 

(defn val [prog x]
  (if (re-matches #"[a-z]" x)
    (get prog x 0)
    (Long/parseLong x)))

(defn parse-instruction [i]
  (if-let [[_ op x y] (re-matches #"(snd|set|add|mul|mod|rcv|jgz) ([a-z]|-?\d+) ?([a-z]|-?\d+)?" i)]
    (case op
      "snd" #(do
               (swap! (:snd %) (fn [q] (conj q (val % x))))
               (update % :snd-cnt inc))
      "set" #(assoc % x (val % y))
      "add" #(update % x (fnil (partial + (val % y)) 0))
      "mul" #(update % x (fnil (partial * (val % y)) 0))
      "mod" #(update % x (fnil (fn [xv] (mod xv (val % y))) 0))
      "rcv" #(if (peek @(:rcv %))
               (-> %
                   (assoc x (-> (swap-vals! (:rcv %) pop) first peek))
                   (assoc :status :run))
               (assoc % :status :wait))
      "jgz" #(if (pos? (val % x)) (update % :pc (fnil (partial + (dec (val % y))) 0)) %))
    (throw (IllegalArgumentException. (format "Cannot parse instruction '%s'" i)))))

(defn run-prog [prog xs]
  (cond
    (= :terminated (:status prog))
    prog

    (not (<= 0 (inc (:pc prog)) (dec (count xs))))
    (assoc prog :status :terminated)

    :else
    (-> prog
        (#(if-not (= :wait (:status prog)) (update % :pc inc) %))
        (#(let [x (xs (:pc %))]
            (x %))))))

(defn duet [instructions]
  (let [prog1s (atom [])
        xs (mapv parse-instruction instructions)
        rcv0 (atom clojure.lang.PersistentQueue/EMPTY)
        rcv1 (atom clojure.lang.PersistentQueue/EMPTY)]
    (loop [prog0 ((first xs) (-> initial-prog
                                 (assoc "p" 0)
                                 (assoc :rcv rcv0)
                                 (assoc :snd rcv1)))
           prog1 ((first xs) (-> initial-prog
                                 (assoc "p" 1)
                                 (assoc :rcv rcv1)
                                 (assoc :snd rcv0)))]
      (or (and (#{:wait :terminated} (:status prog0))
               (#{:wait :terminated} (:status prog1))
               (:snd-cnt prog1))
          (recur (run-prog prog0 xs) (run-prog prog1 xs))))))
