(ns algos-ds-clj.josephus)

(defn- modular-index [index n] (inc (mod (dec index) n)))

(defn kill-at-index
  [warriors index n]
  (let [kill-index (modular-index index n)]
    (into (subvec warriors 0 (dec kill-index))
          (subvec warriors kill-index))))

(defn next-to-kill [index n k] (modular-index (+ index (dec k)) n))

(defn naive-josephus*
  [warriors pointer n k]
  (if (< 1 n)
    (recur (kill-at-index warriors pointer n)
           (next-to-kill pointer (dec n) k)
           (dec n)
           k)
    warriors))

(defn naive-josephus
  [n k]
  (-> (range 1 (inc n))
      vec
      (naive-josephus* (modular-index k n) n k)
      first))

(defn solution-mapping [idx m] (+ idx (quot (dec idx) (dec m))))

(defn reindexing [idx l m] (modular-index (- idx (mod l m)) (- l (quot l m))))

(defn josephus
  "Uses stack efficient base case iteration n < k, which supports effcient logarithmic
   size reduction when n >> k."
  [n k]
  (cond
    (= k 1)  n
    (= n 1)  1
    (<= n k) (reduce (fn [j n'] (modular-index (+ j k) n')) 1 (range 2 (inc n)))
    (< k n)  (-> (- n (quot n k))
                 (josephus  k)
                 (reindexing n k)
                 (solution-mapping k))))
