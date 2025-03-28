(ns algos-ds-clj.coin-change
  (:require [algos-ds-clj.core :as core]))

(defn cons-unless-nil [x v]
  (when-not (nil? v)
    (cons x v)))

(defn count-or-inf [a]
  (if (nil? a) ##Inf (count a)))

(defn optimal-total-coins [amount coin-set]
  (let [coin-change (core/memo-rec-fn coin-change-fn [amount coin-set]
                                      (let [min-coin (first coin-set)
                                            delta (- amount min-coin)]
                                        (cond
                                          (= 0 amount) []
                                          (< delta min-coin) nil
                                          :else
                                          (->> coin-set
                                               (map #(conj [] % (coin-change-fn (- amount %) coin-set)))
                                               (sort-by #(-> % second count-or-inf))
                                               first
                                               (apply cons-unless-nil)))))]
    (some->> coin-set
             sort
             (coin-change amount)
             count)))
