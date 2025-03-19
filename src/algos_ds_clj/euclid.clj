(ns algos-ds-clj.euclid)

(defn naive-gcd [m n]
  (if-not (or (= 0 m) (= 0 n))
    (or
     (->> (range (+ m n 1))
          (drop 2)
          (filter #(and (= 0 (mod n %))
                        (= 0 (mod m %))))
          last)
     1)
    nil))

(defn naive-mcm [m n]
  (cond
    (= 1 (* m n)) 1
    (= 0 (* m n)) 0
    :else
    (->> (range)
         (drop 2)
         (filter #(and (= 0 (mod % n))
                       (= 0 (mod % m))))
         first)))

(defn gcd [m n]
  (if-not (or (= m 0) (= n 0))
    (->> [(max m n) (min m n)]
         (iterate (fn [[a b]] [b (mod a b)]))
         (drop-while #(not= 0 (second %)))
         ffirst)
    nil))

(defn mcm [m n]
  (if-not (or (= 0 m) (= 0 n))
    (-> m
        (* n)
        (/ (gcd m n)))
    0))

