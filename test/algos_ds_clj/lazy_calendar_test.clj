(ns algos-ds-clj.lazy-calendar-test
  (:require
   [algos-ds-clj.lazy-calendar :as cal]
   [clojure.test :refer [deftest are]]))

(deftest day-of-week-test
  (are [day-number date-str]
      (= day-number (-> date-str cal/str->local-date cal/day-of-week))
   0 "2025-04-07"
   1 "2025-04-08"
   2 "2025-04-09"
   3 "2025-04-10"
   4 "2025-04-11"
   5 "2025-04-12"
   6 "2025-04-13"))

(deftest prior-sunday-test
  (are [day-number date-str]
      (= (cal/str->local-date day-number) (-> date-str cal/str->local-date cal/prior-sunday))
   "2024-12-29" "2024-12-30"
   "2024-12-29" "2024-12-31"
   "2024-12-29" "2025-01-01"
   "2024-12-29" "2025-01-02"
   "2024-12-29" "2025-01-03"
   "2024-12-29" "2025-01-04"
   "2025-01-05" "2025-01-05"
   "2025-01-05" "2025-01-06"))
