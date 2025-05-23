(ns algos-ds-clj.core
  (:gen-class))

;; NOTE Taken from stackoveflow answer by miner49r. Not upvoted but works!
;; https://stackoverflow.com/questions/27445876/is-there-a-simpler-way-to-memoize-a-recursive-let-fn
(defmacro memo-rec-fn
  "Returns an anonymous function like `fn` but recursive calls to the given `name` within
      `body` use a memoized version of the function, potentially improving performance (see
      `memoize`).  Only simple argument symbols are supported, not varargs or destructing or
      multiple arities.  Memoized recursion requires explicit calls to `name` so the `body`
      should not use recur to the top level."
  [name args & body]
  {:pre [(simple-symbol? name) (vector? args) (seq args) (every? simple-symbol? args)]}
  (let [akey (if (= (count args) 1) (first args) args)]
    ;; name becomes extra arg to support recursive memoized calls
    `(let [f# (fn [~name ~@args] ~@body)
           mem# (atom {})]
       (fn mr# [~@args]
         (if-let [e# (find @mem# ~akey)]
           (val e#)
           (let [ret# (f# mr# ~@args)]
             (swap! mem# assoc ~akey ret#)
             ret#))))))
