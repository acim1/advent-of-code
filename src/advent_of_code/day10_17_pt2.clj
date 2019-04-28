(ns advent-of-code.day10-17-pt2)

(def lengths (let [lens "197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63"
                   suffix [17,31,73,47,23]]
               (concat (map int lens) suffix)))

(def input [(range 0 256) lengths])

(def test-input [(range 0 256)
                 [49,44,50,44,51,17,31,73,47,23]])

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

(defn knot-hash [[nums ls p s]]
  (loop [xs (transient (vec nums)) pos p skip s
         [len & lens] ls]
    (if-not len
      [(persistent! xs) ls pos skip]
      (recur
       (reverse-length xs pos len)
       (mod (+ pos len skip) (count xs))
       (inc skip)
       lens))))

(defn knot-hash2 [[nums ls]]
  (let [[xs] (last (take 65 (iterate knot-hash
                                     [nums ls 0 0])))
        xors (map #(apply bit-xor %) (partition 16
                                                xs))
        hex (fn [n]
              (let [pad (if (< n 16)
                          "0"
                          "")]
                (str pad (Long/toString n 16))))]
    (clojure.string/join (map hex xors))))
