(ns algos-ds-clj.josephus)

(defn initial-state [n k]
  {:total-people n
   :skip k
   :killed-people #{(dec k)}
   :last-killed (dec k)
   :total-killed 1})

(defn killing-rule [{:keys [total-people skip killed-people last-killed total-killed] :as current-state}]
  (let [next-to-kill (->> (range)
                          (drop-while #(<= % last-killed))
                          (map #(mod % total-people))
                          (remove killed-people)
                          (take skip)
                          last)]
    (assoc current-state
           :killed-people (->> killed-people (cons next-to-kill) set)
           :last-killed next-to-kill
           :total-killed (inc total-killed))))

(defn naive-josephus
  "Naive implementation of josephus problem solution. Intended only for direct
   exploring. Works only for n > k"
  [n k]
  (let [init-state (initial-state n k)]
    (->> init-state
         (iterate killing-rule)
         (drop-while #(< (:total-killed %) n))
         first
         :last-killed)))

(defn josephus
  "Josephus sequence is such that after applying each death, the problem is equivalent
   to a smaller one (n-1), by reindexing so that the first man is the one after the last
   killed. This can be seen by doing the simulation on paper"
  [n k]
  (->> [0 1]
       (iterate (fn [[prev iter]]
                  (let [next-iter (inc iter)]
                    [(-> prev (+ k) (mod next-iter)) next-iter])))
       (drop (dec n))
       ffirst))

;; leave this comment here as evidence of the
;; solution process :)
(comment
  (def sample (initial-state 12 3))
  (def sample (killing-rule sample))
  (killing-rule sample)
  (naive-josephus 11 19)
  (josephus 17 8)

  (->> (range 2 17)
       (map (fn [n]
              {:n n
               :josephus (naive-josephus n 2)})))

  (into (sorted-map-by <)
        (-> (->> (range 3 20)
                 (map (fn [n]
                        {:n n
                         :josephus (naive-josephus n 4)}))
                 (group-by :josephus))
            (update-vals #(map :n %)))))
