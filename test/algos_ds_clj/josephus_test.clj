(ns algos-ds-clj.josephus-test
  (:require
   [algos-ds-clj.josephus :as jos]
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]))

(deftest naive-josephus-test
  (testing "When validating common values n > k"
    (is (= 3 (jos/naive-josephus 7 3)))
    (is (= 0 (jos/naive-josephus 16 2)))
    (is (= 14 (jos/naive-josephus 15 2)))
    (is (= 12 (jos/naive-josephus 14 2)))))

(deftest josephus-test
  (testing "When validating common values"
    (is (= 3 (jos/josephus 7 3)))
    (is (= 9 (jos/josephus 11 19)))
    (is (= 0 (jos/josephus 1 300)))
    (is (= 12 (jos/josephus 14 2)))))

(defspec josephus-stress-test
  {:num-tests 65}
  (prop/for-all [n (gen/such-that #(< 2 %) gen/nat)]
                (= (jos/josephus n 2)
                   (jos/naive-josephus n 2))))
