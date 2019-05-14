(ns advent-of-code.day15-17-pt1)

(def divisor 2147483647)

(def input [[703 16807 divisor] [516 48271 divisor]])

(def test-input [[65 16807 divisor] [8921 48271 divisor]])

(defn lower-bytes [n x]
  (let [bs (.toByteArray (BigInteger. (str x)))]
    (if (< (count bs) n)
      (BigInteger. bs)
      (BigInteger. (java.util.Arrays/copyOfRange bs (- (count bs) n) (count bs))))))

(defn generate [generator-inputs]
(for [[previous factor divisor] generator-inputs]
  [(rem (* previous factor) divisor) factor divisor]))

(defn dueling-generators [initial-generator-inputs n]
(count (filter (fn [outputs] (apply = (map (comp (partial lower-bytes 2) first) outputs)))
               (take n (rest (iterate generate initial-generator-inputs))))))

