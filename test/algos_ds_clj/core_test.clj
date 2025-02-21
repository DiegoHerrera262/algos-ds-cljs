(ns algos-ds-clj.core-test
  (:require [clojure.test :refer :all]
            [algos-ds-clj.core :refer :all]))

(deftest sample-test
  (testing "creating a new test for clojure"
    (is (= 1 2))))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest some-test
  (testing "New tests"
    (is (= 1 1))))

(defn my-sample
  "Some sample fun that receives some args to produce an output"
  [& args]
  (count args))

