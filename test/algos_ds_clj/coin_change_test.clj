(ns algos-ds-clj.coin-change-test
  (:require [algos-ds-clj.coin-change :as coin]
            [clojure.test :refer :all]))

(deftest cons-unless-nil-test
  (testing "When vector to cons to is nil"
    (is (= nil (coin/cons-unless-nil 5 nil))))
  (testing "When vector to cons to is not nil"
    (is (= [5] (coin/cons-unless-nil 5 [])))))

(deftest count-or-inf-test
  (testing "When to count from is nil"
    (is (= ##Inf (coin/count-or-inf nil))))
  (testing "When to count to is seq"
    (is (= 0 (coin/count-or-inf [])))))

(deftest simple-memo-coin-change-test
  (testing "When solution is found"
    (are [expected amount coins] (= expected (coin/optimal-total-coins amount coins))
      2 6 [1 2 3]
      3 13 [5 4 2]
      3 11 [1 2 5]
      30 150 [1 4 5]
      12 3239 [195 265 404 396]
      20 6249 [186 419 83 408]))
  (testing "When solution is not found"
    (is (= nil (coin/optimal-total-coins 13 [10 2]))))
  (testing "When amount to parition is 0"
    (is (= 0 (coin/optimal-total-coins 0 [1 2 5])))))

