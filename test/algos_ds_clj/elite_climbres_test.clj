(ns algos-ds-clj.elite-climbers-test
  (:require  [clojure.test :refer [is are testing deftest]]
             [algos-ds-clj.elite-climbers :as elite-climbers]
             [clojure.test.check.properties :as prop]
             [clojure.test.check.clojure-test :refer [defspec]]
             [clojure.test.check.generators :as gen]))

(deftest climber-cant-return?-test
  (are [cant-return? climber] (= cant-return? (elite-climbers/climber-cant-return? climber))
    true  #:climber{:height 10 :capacity 8}
    false #:climber{:height 10 :capacity 12}))

(deftest climber-cant-ascend?-test
  (are [cant-return? climber] (= cant-return? (elite-climbers/climber-cant-ascend? climber))
    false #:climber{:height 5 :capacity 8}
    false #:climber{:height 5 :capacity 7}
    true  #:climber{:height 1 :capacity 2}))

(defspec optimal-ascending-sterss-test
  {:num-tests 100}
  (prop/for-all [capacity   (gen/such-that pos? gen/nat)
                 group-size (gen/such-that pos? gen/nat)]
                (= (elite-climbers/fast-optimal-ascent-length group-size capacity)
                   (-> (elite-climbers/optimal-ascent group-size capacity)
                       elite-climbers/ascent-length))))
