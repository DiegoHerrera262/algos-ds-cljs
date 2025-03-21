(ns algos-ds-clj.sorting)

(defn insert-in-sorted [sorted to-insert]
  (let [{:keys [inserted? new-sorted]} (reduce
                                        (fn [{:keys [inserted? new-sorted]} element]
                                          {:inserted? (or inserted? (<= to-insert element))
                                           :new-sorted (if (and (not inserted?) (<= to-insert element))
                                                         (conj new-sorted to-insert element)
                                                         (conj new-sorted element))})
                                        {:inserted? false :new-sorted []}
                                        sorted)]
    (if inserted? new-sorted (conj new-sorted to-insert))))

(defn insertion-sort [to-sort]
  (if (empty? to-sort) [] (reduce insert-in-sorted [(first to-sort)] (rest to-sort))))

(defn merge-subarrays [array-1 array-2]
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
    (if (<= el-count 1)
      to-sort
      (let [[left-array right-array] (split-at (-> el-count (/ 2) int) to-sort)]
        (merge-subarrays
         (merge-sort left-array)
         (merge-sort right-array))))))
