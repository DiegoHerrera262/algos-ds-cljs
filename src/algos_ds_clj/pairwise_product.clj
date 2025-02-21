(ns algos-ds-clj.pairwise-product)

(defn max-with-index-filter
  [my-seq filter-idx]
  (when (not-empty my-seq)
    (reduce-kv (fn [[max-val _ :as max-tuple] idx el]
                 (if (and (or (not (number? filter-idx)) (not= idx filter-idx)) (<= max-val el))
                   [el idx]
                   max-tuple))
               (if (and (number? filter-idx) (= 0 filter-idx))
                 [(second my-seq) 1]
                 [(first my-seq) 0])
               my-seq)))

(defn max-pairwise-product [my-seq]
  (->> [nil nil]
       (iterate #(max-with-index-filter my-seq (second %)))
       (take 3) ;; just because the first element is the input
       rest
       (map (comp bigdec first))
       (reduce *)))

(defn brute-max-pairwise-product [my-seq]
  (when (not-empty my-seq)
    (let [products (map-indexed (fn [idx numb]
                                  (->> my-seq
                                       (map-indexed
                                        (fn [jdx numb*]
                                          (when (not= jdx idx)
                                            (* (bigdec numb*) (bigdec numb)))))
                                       (filter some?))) my-seq)]
      (apply max (flatten products)))))

(comment
  (max-pairwise-product [100 -1 3 -30 3 100])
  (max-with-index-filter [3 100 4 -1] 1)
  (reduce-kv (fn [m k v]
               (println m k v)
               (cons v m))
             []
             [1 2 3 4]))
