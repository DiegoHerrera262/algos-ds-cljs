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

(deftest insert-in-sorted-test
  (testing "When to insert is not in array"
    (is (= [1 2 3 4 5 6 7 8] (sorting/insert-in-sorted [1 2 3 4 5 7 8] 6))))
  (testing "When to insert is in array"
    (is (= [1 2 3 3 4] (sorting/insert-in-sorted [1 2 3 4] 3))))
  (testing "When to insert should be in first position"
    (is (= [0 1 2 3] (sorting/insert-in-sorted [1 2 3] 0))))
  (testing "When to insert should be in last position"
    (is (= [0 1 2 3] (sorting/insert-in-sorted [0 1 2] 3)))))

(deftest merge-subarrays-test
  (testing "When all elements are different"
    (is (= [1 2 3 4 5 6] (sorting/merge-subarrays [2 4 6] [1 3 5]))))
  (testing "When there are repeated elements"
    (is (= [1 1 2 2 3 3 4 4] (sorting/merge-subarrays [1 2 3] [1 2 3 4 4]))))
  (testing "When left is empty"
    (is (= [1 2 3] (sorting/merge-subarrays [] [1 2 3]))))
  (testing "When right is empty"
    (is (= [1 2 3] (sorting/merge-subarrays [1 2 3] []))))
  (testing "When both are empty"
    (is (= [] (sorting/merge-subarrays [] [])))))

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
