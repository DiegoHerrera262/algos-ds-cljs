(ns algos-ds-clj.sorting)

(defn- insert-in-sorted [sorted to-insert]
  (let [{:keys [inserted? new-sorted]} (reduce
                                        (fn [{:keys [inserted? new-sorted]} element]
                                          {:inserted? (or inserted? (<= to-insert element))
                                           :new-sorted (cond
                                                         (< element to-insert) (conj new-sorted element)
                                                         (and (not inserted?) (<= to-insert element)) (conj new-sorted to-insert element)
                                                         :else (conj new-sorted element))})
                                        {:inserted? false :new-sorted []}
                                        sorted)]
    (if inserted? new-sorted (conj new-sorted to-insert))))

(defn insertion-sort [to-sort]
  (if (empty? to-sort) [] (reduce insert-in-sorted [(first to-sort)] (rest to-sort))))

(defn- merge-subarrays [array-1 array-2]
  (loop [left-array array-1
         right-array array-2
         merge-array []]
    (let [min-of-left (first left-array)
          min-of-right (first right-array)]
      (cond
        (empty? right-array) (concat merge-array left-array)
        (empty? left-array) (concat merge-array right-array)
        (<= min-of-left min-of-right) (recur (drop 1 left-array)
                                             right-array
                                             (conj (vec merge-array) min-of-left))
        (<= min-of-right min-of-left) (recur left-array
                                             (drop 1 right-array)
                                             (conj (vec merge-array) min-of-right))))))

(defn merge-sort [to-sort]
  (let [el-count (count to-sort)]
    (cond
      (empty? to-sort) to-sort
      (= el-count 1) to-sort
      :else
      (let [[left-array right-array] (split-at (-> el-count (/ 2) int) to-sort)]
        (merge-subarrays
         (merge-sort left-array)
         (merge-sort right-array))))))

(comment
  (merge-subarrays [2 4 6] [1 3 5])
  (merge-subarrays [2 4 6] [-1 2 4])
  (merge-subarrays [2] [3])
  (merge-subarrays [1 3 5] [-1])
  (merge-subarrays [] [])
  (merge-subarrays [] [1 2 3])
  (merge-subarrays [1 2 3] [])

  (insertion-sort [10 2 10 1 2 3 4 1])
  (insertion-sort [-1 2 0 -1.5 -1.5 10 1 2.4])

  (-> 7 (/ 2) int)
  (partition 3 (range 7))
  (first (merge-sort (range 1000000)))
  (first (sort (range 1000000)))
  (merge-sort [6 7 10 1 8 5 3 2 9 4])
  (merge-sort [10 2 10 1 2 3 4 1])
  (merge-sort [2 10 3 1 4 1 10 2])
  (merge-sort [20.5 0 1 -1 2.4 10 -1.2]))
