(ns algos-ds-clj.fibonacci-numbers-test
  (:require
   [algos-ds-clj.fibonacci-numbers :as fib]
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]))

(deftest fibonacci-test
  (testing "Lazy fibonacci seq is correct"
    (is (= 0M (fib/fibonacci 0)))
    (is (= 1M (fib/fibonacci 1)))
    (is (= 2M (fib/fibonacci 3)))
    (is (= 55M (fib/fibonacci 10)))
    (is (= 1134903170M (fib/fibonacci 45)))
    (is (= 1548008755920M (fib/fibonacci 60M)))))

(deftest modular-fibonacci-test
  (testing "Lazy modular fibonacci with period truncation is correct"
    (is (= 4 (fib/modular-fibonacci 9 10)))
    (is (= 5 (fib/modular-fibonacci 10 10)))
    (is (= 885 (fib/modular-fibonacci 115 1000)))
    (is (= 151 (fib/modular-fibonacci 2816213588 239)))))

(defspec modular-equivalent-to-normal
  {:num-tests 65}
  (prop/for-all [idx (gen/such-that #(<= % 65) gen/nat)]
                (= (-> (fib/fibonacci idx) (mod 10))
                   (-> (fib/modular-fibonacci idx 10) bigdec))))

(defspec fibonacci-sum-spec
  {:num-tests 65}
  (prop/for-all [idx (gen/such-that #(<= % 65) gen/nat)]
                (= (fib/naive-fibonacci-sum idx)
                   (fib/fibonacci-sum idx))))

(defspec modular-fibonacci-sum-spec
  {:num-tests 65}
  (prop/for-all [idx (gen/such-that #(<= % 65) gen/nat)]
                (= (-> (fib/fibonacci-sum idx) (mod 10))
                   (-> (fib/modular-fibonacci-sum idx 10) bigdec))))

(defspec square-fibonacci-sum-spec
  {:num-tests 65}
  (prop/for-all [idx (gen/such-that #(<= % 65) gen/nat)]
                (= (fib/naive-fibonacci-square-sum idx)
                   (fib/square-fibonacci-sum idx))))

(defspec modular-square-fibonacci-sum-spec
  {:num-tests 65}
  (prop/for-all [idx (gen/such-that #(<= % 65) gen/nat)]
                (= (-> (fib/square-fibonacci-sum idx) (mod 10))
                   (-> (fib/modular-square-fibonacci-sum idx 10) bigdec))))
