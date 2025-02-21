(ns algos-ds-clj.fibonacci-numbers-test
  (:require [algos-ds-clj.fibonacci-numbers :as fib]
            [clojure.test :refer [testing is deftest]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest fibonacci-recursion-test
  (testing "Recusion works correctly"
    (is (= [3M 2M] (fib/fibonacci-recursion [2 1])))))

(deftest fibonacci-numbers-test
  (testing "Lazy fibonacci seq is correct"
    (is (= '(1M 1M 2M 3M 5M 8M 13M 21M) (->> fib/fibonacci-numbers (take 8))))
    (is (= 2M (fib/nth-fibonacci 3)))
    (is (= 55M (fib/nth-fibonacci 10))))

  (testing "Can compute large fibonacci numbers"
    (is (= 13598018856492162040239554477268290M (fib/nth-fibonacci 165)))
    (is (= 1M (-> (fib/nth-fibonacci 139) (mod 10))))
    (is (= 6M (-> (fib/nth-fibonacci 91239) (mod 10M))))))

(defspec fibonacci-sum-spec
  {:num-tests 200}
  (prop/for-all [idx (gen/such-that #(<= 1 % 200) gen/nat)]
                (= (->> fib/fibonacci-numbers (take idx) (reduce +))
                   (fib/nth-fibonacci-sum idx))))

(defspec fibonacci-squares-sum-spec
  {:num-tests 200}
  (prop/for-all [idx (gen/such-that #(<= 1 % 200) gen/nat)]
                (= (->> fib/fibonacci-numbers (map #(* % %)) (take idx) (reduce +))
                   (fib/nth-fibonacci-square-sum idx))))
