(ns algos-ds-clj.josephus-test
  (:require
   [algos-ds-clj.josephus :as jos]
   [clojure.test :refer [are deftest testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]))

(deftest kill-at-index-test
  (letfn [(kill-at-index* [survivors index]
            (jos/kill-at-index survivors index (count survivors)))]
    (testing "When considering index INSIDE boundaries"
      (are [final-survivors previous-survivors to-kill]
           (= final-survivors (kill-at-index* previous-survivors to-kill))
        [2 3 4 5] [1 2 3 4 5] 1
        [1 3 4 5] [1 2 3 4 5] 2
        [1 2 4 5] [1 2 3 4 5] 3
        [1 2 3 5] [1 2 3 4 5] 4
        [1 2 3 4] [1 2 3 4 5] 5))
    (testing "When considering index OUTSIDE boundaries"
      (are [final-survivors previous-survivors to-kill]
           (= final-survivors (kill-at-index* previous-survivors to-kill))
        [2 3 4 5] [1 2 3 4 5] 6
        [1 3 4 5] [1 2 3 4 5] 7
        [1 2 4 5] [1 2 3 4 5] 8
        [1 2 3 5] [1 2 3 4 5] 9
        [1 2 3 4] [1 2 3 4 5] 10))))

(deftest next-to-kill
  (testing "With a fully worked example, with k = 5 = n
            n = 5, [0 1 2 3 4]
            n = 4, [0 1 2 3]
            n = 3, [1 2 3]
            n = 2, [1 3]
            n = 1, [1]"
    (are [new-index prev-index n] (= new-index (jos/next-to-kill prev-index n 5))
      1 5 4
      2 1 3
      2 2 2))
  (testing "With a fully worked example, with k = 5 < 7 = n
            n = 7, [0 1 2 3 4 5 6]
            n = 6, [0 1 2 3 5 6]
            n = 5, [0 1 3 5 6]
            n = 4, [0 3 5 6]
            n = 3, [0 5 6]
            n = 2, [0 5]
            n = 1, [5]"
    (are [new-index prev-index n] (= new-index (jos/next-to-kill prev-index n 5))
      3 5 6
      2 3 5
      2 2 4
      3 2 3
      1 3 2))
  (testing "With a fully worked example, with k = 5 > 4 = n
            n = 4, [0 1 2 3]
            n = 3, [1 2 3]
            n = 2, [1 3]
            n = 1, [1]"
    (are [new-index prev-index n] (= new-index (jos/next-to-kill prev-index n 5))
      2 1 3
      2 2 2)))

(deftest solution-mapping-test
  (testing "Precalculated examples"
    (are [original-space reindexed-space k]
         (= original-space (mapv #(jos/solution-mapping % k) reindexed-space))
      [1 2 3 5 6 7 9 10 11 13 14 15]       [1 2 3 4 5 6 7 8 9 10 11 12]       4
      [1 2 3 5 6 7 9 10 11 13 14 15 17 18] [1 2 3 4 5 6 7 8 9 10 11 12 13 14] 4
      [1 2 4 5 7 8]                        [1 2 3 4 5 6]                      3
      [1 2 3 4 6 7 8 9]                    [1 2 3 4 5 6 7 8]                  5
      [1 3 5 7 9]                          [1 2 3 4 5]                        2
      [1 2 4]                              [1 2 3]                            3)))

(deftest reindexing-test
  (testing "Precalculated examples"
    (are [original-space reindexed-space n k]
         (= original-space (mapv #(jos/reindexing % n k) reindexed-space))
      [1 2 3 4 5 6 7 8 9 10 11 12 13 14] [3 4 5 6 7 8 9 10 11 12 13 14 1 2] 18 4
      [1 2 3 4 5 6 7 8 9 10 11 12]       [1 2 3 4 5 6 7 8 9 10 11 12]       16 4
      [1 2 3 4 5 6 7 8 9 10 11]          [4 5 6 7 8 9 10 11 1 2 3]          13 5
      [1 2 3 4 5 6 7 8 9 10 11]          [2 3 4 5 6 7 8 9 10 11 1]          16 3
      [1 2 3]                            [2 3 1]                            4  3)))

(deftest josephus-comparison-test
  (testing "When comparing naive and refined solutions"
    (are [expected n k]
         (= expected (jos/naive-josephus n k) (jos/josephus n k))
      4  7  3
      1  4  3
      1  16 2
      5  15 7
      10 11 19
      1  1  300
      11 14 5
      7  7  1)))

(defspec josephus-stress-test
  {:num-tests 65}
  (prop/for-all [n (gen/choose 1 500)
                 k (gen/choose 1 500)]
                (= (jos/josephus n k) (jos/naive-josephus n k))))
