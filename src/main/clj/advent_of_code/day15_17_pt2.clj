(ns advent-of-code.day15-17-pt2)

(def divisor 2147483647)

(def input [[703 16807 divisor] [516 48271 divisor]])

(def test-input [[65 16807 divisor] [8921 48271 divisor]])

(defn lower-bytes [n x]
  (let [bs (.toByteArray (BigInteger. (str x)))]
    (if (< (count bs) n)
      (BigInteger. bs)
      (BigInteger. (java.util.Arrays/copyOfRange bs (- (count bs) n) (count bs))))))

(defn generate [[previous factor divisor]]
  [(rem (* previous factor) divisor) factor divisor])

(defn dueling-generators [[a b] n]
  (count (filter #(apply = (map (comp (partial lower-bytes 2) first) %))
                 (take n (map vector
                              (filter #(zero? (mod (first %) 4)) (rest (iterate generate a)))
                              (filter #(zero? (mod (first %) 8)) (rest (iterate generate b))))))))

