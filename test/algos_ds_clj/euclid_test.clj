(ns algos-ds-clj.euclid-test
  (:require
   [algos-ds-clj.euclid :as euclid]
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]))

(deftest gdc-test
  (testing "gdc is computed correctly for some values"
    (is (= 21 (euclid/gcd 1071 462)))
    (is (= 4 (euclid/gcd 12 4)))
    (is (= 17657 (euclid/gcd 28851538 1183019)))))

(deftest mcm-test
  (testing "mcm is computed correctly for some values"
    (is (= 12 (euclid/mcm 3 4)))
    (is (= 6 (euclid/mcm 6 3)))
    (is (= 467970912861 (euclid/mcm 761457 614573)))))

(defspec gdc-stress-test
  {:num-tests 100}
  (prop/for-all [m (gen/such-that #(<= % 65) gen/nat)
                 n (gen/such-that #(<= % 65) gen/nat)]
                (= (euclid/gcd m n)
                   (euclid/naive-gcd m n))))

(defspec mcm-stress-test
  {:num-tests 100}
  (prop/for-all [m (gen/such-that #(<= % 65) gen/nat)
                 n (gen/such-that #(<= % 65) gen/nat)]
                (= (euclid/mcm m n)
                   (euclid/naive-mcm m n))))
