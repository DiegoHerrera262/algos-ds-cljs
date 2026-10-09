(ns algos-ds-clj.pairwise-product-test
  (:require
   [algos-ds-clj.pairwise-product :as pp]
   [clojure.test :refer [deftest is are testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]))

(deftest max-knockout-test
  (testing "When less than second max"
    (is (= [10 11] (pp/max-knockout [10 11] 2))))
  (testing "When less than max but grater or equal than second max"
    (is (= [13 12] (pp/max-knockout [13 11] 12)))
    (is (= [13 11] (pp/max-knockout [13 11] 11))))
  (testing "When greater or equal than max"
    (is (= [14 13] (pp/max-knockout [13 11] 14)))
    (is (= [13 13] (pp/max-knockout [13 11] 13)))))

(deftest max-pairwise-product-test
  (are [num-vec max-prod] (= (bigdec max-prod) (pp/max-pairwise-product num-vec))
    [10 9 11 13 14 20]      280
    [7 5 14 2 8 8 10 1 2 3] 140
    [9 8 7 9 1 4 5]         81))

(deftest brute-max-pairwise-product-test
  (are [num-vec max-prod] (= (bigdec max-prod) (pp/brute-max-pairwise-product num-vec))
    [10 9 11 13 14 20]      280
    [7 5 14 2 8 8 10 1 2 3] 140
    [9 8 7 9 1 4 5]         81))

(defspec max-pairwise-product-consistency
  {:num-tests 1000}
  (prop/for-all [int-seq (gen/not-empty (gen/vector (gen/large-integer* {:min 0}) 2 50))]
                (= (pp/max-pairwise-product int-seq) (pp/brute-max-pairwise-product int-seq))))

