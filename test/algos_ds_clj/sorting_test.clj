(ns algos-ds-clj.sorting-test
  (:require
   [algos-ds-clj.sorting :as sorting]
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]))

(defmacro sort-spec [sort-fn]
  `(prop/for-all [v# (gen/not-empty (gen/vector gen/pos-int 0 100))]
                 (let [sorted-v# (~sort-fn v#)]
                   (and
                   ;; preserves number of elements
                    (= (count v#) (count sorted-v#))
                   ;; idempotent
                    (= sorted-v# (~sort-fn sorted-v#))
                   ;; monotonically increasing
                    (->> sorted-v# (map - (rest sorted-v#)) (reduce + 0) (<= 0))))))

(defmacro sort-test [sort-fn]
  `(testing "When example testing"
     (testing "Base case with all different numbers"
       (is (= '(1 2 3 4 5 6 7 8 9 10) (~sort-fn [6 7 10 1 8 5 3 2 9 4]))))
     (testing "When numbers are repeated in the array"
       (is (= '(1 1 2 2 3 4 10 10) (~sort-fn [2 10 3 1 4 1 10 2]))))
     (testing "When array has negative and floating point numbers"
       (is (= '(-1.2 -1 0 1 2.4 10 20.5) (~sort-fn [20.5 0 1 -1 2.4 10 -1.2]))))
     (testing "When array is empty"
       (is (= '() (~sort-fn []))))))

(deftest insertion-sort-test
  (sort-test sorting/insertion-sort))

(defspec insertion-sort-spec
  {:num-tests 100}
  (sort-spec sorting/insertion-sort))

(deftest merge-sort-test
  (sort-test sorting/merge-sort))

(defspec merge-sort-spec
  {:num-tests 100}
  (sort-spec sorting/merge-sort))
