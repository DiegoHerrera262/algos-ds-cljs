(ns algos-ds-clj.fibonacci-numbers
  (:require [algos-ds-clj.fibonacci-numbers :as fib]))

(defn fibonacci-recursion [[prev-term :as previous-two]]
  [(bigdec (reduce + previous-two)) (bigdec prev-term)])

(def fibonacci-seed [1M 0M])

(def fibonacci-numbers
  (->> fibonacci-seed
       (iterate fibonacci-recursion)
       (map first)))

(def sum-fibonacci-numbers
  "F_n+2 - 1 = Sum(F_m) m=0 -> m=n-2"
  (->> fibonacci-numbers
       (drop 2)
       (map dec)))

(def sum-fibonacci-squares-numbers
  "F_n * F_n-1 = Sum(F_m^2) m=0 -> m = n-1"
  (->> fibonacci-seed
       (iterate fibonacci-recursion)
       (drop 1)
       (map (partial reduce *))))

(defn nth-fibonacci-seq [fib-seq n]
  (->> fib-seq
       (take n)
       last))

(def nth-fibonacci (partial nth-fibonacci-seq fibonacci-numbers))
(def nth-fibonacci-sum (partial nth-fibonacci-seq sum-fibonacci-numbers))
(def nth-fibonacci-square-sum (partial nth-fibonacci-seq sum-fibonacci-squares-numbers))
