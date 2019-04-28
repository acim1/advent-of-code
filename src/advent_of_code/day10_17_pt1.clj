(ns advent-of-code.day10-17-pt1)

(def input [(range 0 256)
            [197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63]])
(def test-input [(range 0 5) [3,4,1,5]])

(defn reverse-length [xs index n]
  (let [k (count xs)
        wrap #(condp = %
                -1
                (dec k)
                k
                0
                %)]
    (loop [i index ii
           (mod (+ index (dec n)) (count xs))
           xs xs n (quot n 2)]
      (if (zero? n)
        xs
        (let [v1 (xs i)
              v2 (xs ii)]
          (recur
           (wrap (inc i))
           (wrap (dec ii))
           (-> xs (assoc! i v2) (assoc! ii v1))
           (dec n)))))))

(defn knot-hash [[nums lens]]
  (loop [xs (transient (vec nums)) pos 0 skip 0
         [len & lens] lens]
    (if-not len
      (* (xs 0) (xs 1))
      (recur
       (reverse-length xs pos len)
       (mod (+ pos len skip) (count xs))
       (inc skip)
       lens))))
