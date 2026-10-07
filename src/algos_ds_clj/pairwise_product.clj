(ns algos-ds-clj.pairwise-product)

(defn max-knockout [[max max-2 :as max-vals] number]
  (cond
    (< number max-2) max-vals
    (< number max)   [max number]
    :else            [number max]))

(defn max-pairwise-product [my-seq]
  (->> my-seq
       (reduce max-knockout [-1 -1])
       (map bigdec)
       (apply *)))

(defn brute-max-pairwise-product [my-seq]
  (apply max (for [[i x] (map-indexed vector my-seq)
                   [j y] (map-indexed vector my-seq)
                   :when (< i j)]
               (* (bigdec x) (bigdec y)))))

