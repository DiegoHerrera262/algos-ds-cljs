(ns algos-ds-clj.fibonacci-numbers)

(def fibonacci-numbers
  (->> [0M 1M]
       (iterate (fn [[pre-prev prev]]
                  (->> [prev (+ prev pre-prev)]
                       (map bigdec))))))

(defn modular-fibonacci-numbers
  "Fibonacci seq is periodic in modular arithmetic. The period is not
   easy to calculate but can be found when the pattern ... 0 1 ...
   repeats"
  [equiv]
  (->> [1 1 1]
       (iterate (fn [[pre-prev prev iteration]]
                  [prev (-> prev (+ pre-prev) (mod equiv)) (inc iteration)]))
       (take-while #(-> (= 0 (first %))
                        (and (= 1 (second %)))
                        not))
       (cons [0 1 0])))

(defn modular-fibonacci-index [index equiv]
  (->> (modular-fibonacci-numbers equiv)
       count
       (mod index)))

(defn fibonacci [n]
  (->> fibonacci-numbers
       (drop n)
       ffirst))

(defn naive-fibonacci-sum [n]
  (->> fibonacci-numbers
       (take (inc n))
       (map first)
       (reduce +)))

(defn fibonacci-sum [n]
  (-> n (+ 2) fibonacci dec))

(defn naive-fibonacci-square-sum [n]
  (->> fibonacci-numbers
       (take (inc n))
       (map (comp #(* % %) first))
       (reduce +)))

(defn square-fibonacci-sum [n]
  (->> fibonacci-numbers
       (drop n)
       first
       (reduce *)))

(defn modular-fibonacci [n m]
  (let [m-index (modular-fibonacci-index n m)]
    (->> (modular-fibonacci-numbers m)
         (filter #(= (nth % 2) m-index))
         ffirst)))

(defn to-mod-num [n m]
  (if (<= m n)
    (mod n m)
    (cond-> n (< n 0) (+ m))))

(defn modular-fibonacci-sum [n m]
  (-> n
      (+ 2)
      (modular-fibonacci m)
      dec
      (to-mod-num m)))

(defn modular-square-fibonacci-sum [n m]
  (let [m-index (modular-fibonacci-index n m)
        raw-mod (->> (modular-fibonacci-numbers m)
                     (filter #(= (nth % 2) m-index))
                     first
                     (take 2)
                     (reduce *))]
    (to-mod-num raw-mod m)))

