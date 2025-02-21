(ns algos-ds-clj.pairwise-product-test
  (:require
   [algos-ds-clj.pairwise-product :as pp]
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]))

(deftest max-and-filter-test
  (testing "When filter-idx is 0"
    (let [[max-val max-index] (pp/max-with-index-filter [100 10 4 2] 0)]
      (is (= 10 max-val))
      (is (= 1 max-index))))

  (testing "When filter-idx matches with repeated max value"
    (let [[max-val max-index] (pp/max-with-index-filter [1 2 3 4 5 5] 4)]
      (is (= 5 max-val))
      (is (= 5 max-index))))

  (testing "When empty seq"
    (is (nil? (pp/max-with-index-filter nil [])))))

(defspec max-and-filter-extends-max
  {:num-tests 100}
  (prop/for-all [int-seq (gen/not-empty (gen/vector gen/large-integer))]
                (and
                 (= (apply max int-seq) (first (pp/max-with-index-filter int-seq (-> int-seq count inc))))
                 (= (apply max int-seq) (first (pp/max-with-index-filter int-seq nil))))))

(defspec max-pairwise-product-consistency
  {:num-tests 1000}
  (prop/for-all [int-seq (gen/not-empty (gen/vector (gen/large-integer* {:min 0}) 2 50))]
                (= (pp/max-pairwise-product int-seq) (pp/brute-max-pairwise-product int-seq))))

