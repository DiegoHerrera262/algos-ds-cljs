(ns algos-ds-clj.elite-climbers
  (:require [algos-ds-clj.core :as core]
            [clojure.pprint :refer [pprint]]))

(defn climber-cant-return?
  [{:climber/keys [height capacity]}]
  (< capacity height))

(defn climber-cant-ascend?
  [{:climber/keys [height capacity]}]
  (< (- capacity height) 2))

(def climber-ascending? (comp boolean #{:ascending} :climber/direction))
(def climber-descending? (comp boolean #{:descending} :climber/direction))

(defn reached-summit?
  [{:climbing/keys [climbers]}]
  (every? climber-cant-ascend? climbers))

(defn some-climber-can-ascend?
  ([climbing]
   (some-climber-can-ascend? climbing 1000))
  ([{:climbing/keys [safe? day] :as climbing} max-days]
   (and safe?
        (<= day max-days)
        (not (reached-summit? climbing)))))

(defn safe-climbing?
  [climb]
  (->> climb
       :climbing/climbers
       (some climber-cant-return?)
       not))

(defn climb-journey
  [journey-fn climbing]
  (letfn [(assert-safety [climb]
            (assoc climb :climbing/safe? (safe-climbing? climb)))]
    (-> climbing
        journey-fn
        (update :climbing/day inc)
        assert-safety)))

(defn move-climbers
  [climbing]
  (letfn [(safe-dec [n] (cond-> n (< 0 n) dec))
          (climb-fn [n direction] (condp = direction
                                    :ascending  (inc n)
                                    :descending (safe-dec n)))
          (move-climber [{:climber/keys [direction] :as climber}]
            (-> climber
                (update :climber/height climb-fn direction)
                (update :climber/capacity safe-dec)))]
    (update climbing :climbing/climbers #(map move-climber %))))

(defn simulate-climbing
  [relay-fn initial-set-up]
  (let [relay-and-move-fn (comp relay-fn move-climbers)]
    (->> initial-set-up
         (iterate (partial climb-journey relay-and-move-fn))
         (core/take-while+ some-climber-can-ascend?))))

(defn climber-unused-capacity
  [{:climber/keys [max-capacity capacity]}]
  (- max-capacity capacity))

(defn force-climber-return
  [climber]
  (cond-> climber
    (climber-cant-ascend? climber) (assoc :climber/direction :descending)))

(defn optimum-relay-strategy
  [{:climbing/keys [climbers] :as climbing}]
  (let [{ascending  true
         descending false}                (group-by climber-ascending? climbers)
        leaders-by-capacity               (->> ascending
                                               (sort-by (comp - :climber/capacity)))
        without-leading-climber           (rest leaders-by-capacity)
        {relay-capacity :climber/capacity
         relay-height   :climber/height
         relay-index    :climber/index}   (first without-leading-climber)
        is-relay-climber?                 #(= relay-index (:climber/index %))
        to-distribute                     (if relay-index (- relay-capacity relay-height) 0)
        unused-capacity                   (->> leaders-by-capacity
                                               (remove is-relay-climber?)
                                               (reduce #(+ %1 (climber-unused-capacity %2)) 0))
        extra-capacity*                   (atom to-distribute)]
    (cond-> climbing
      (and (< 0 unused-capacity)
           (<= (- to-distribute unused-capacity) 1))
      (assoc :climbing/climbers (map (fn [climber]
                                       (cond
                                         (is-relay-climber? climber)
                                         (-> climber
                                             (update :climber/capacity - to-distribute)
                                             (assoc :climber/direction :descending))
                                         (climber-descending? climber)
                                         climber
                                         :else
                                         (let [unused-climber-capacity (climber-unused-capacity climber)
                                               to-take-from-extra      (min @extra-capacity* unused-climber-capacity)]
                                           (swap! extra-capacity* #(- % to-take-from-extra))
                                           (-> climber
                                               (update :climber/capacity + to-take-from-extra)
                                               force-climber-return))))
                                     (concat leaders-by-capacity descending))))))

(defn set-up-climbing
  [num-climbers max-capacity]
  (letfn [(set-up-climber [idx] {:climber/direction    :ascending
                                 :climber/height       0
                                 :climber/max-capacity max-capacity
                                 :climber/capacity     max-capacity
                                 :climber/index        idx})]
    {:climbing/day            0
     :climbing/safe?          true
     :climbing/climbers       (->> (range num-climbers)
                                   (map set-up-climber))}))

(defn optimal-ascent
  [total-climbers max-climber-capacity]
  (simulate-climbing optimum-relay-strategy (set-up-climbing total-climbers max-climber-capacity)))

(defn ascent-length
  [climbings]
  (let [last-climbing (last climbings)]
    (if (:climbing/safe? last-climbing)
      (:climbing/day last-climbing)
      -1)))

(defn ^:private reduced-climber-capacity
  [size capacity optimal-breakpoint]
  (let [raw-reduced-capacity (Math/floor (- (* capacity size (/ size (- size 1)))
                                            (* optimal-breakpoint (/ (+ size 1) (- size 1)))))]
    (-> capacity
        (min raw-reduced-capacity)
        (- optimal-breakpoint)
        int)))

(defn fast-optimal-ascent-length
  [total-climbers max-climber-capacity]
  (if (= 1 total-climbers)
    (int (Math/floor (/ max-climber-capacity 2)))
    (let [optimal-breakpoint (int (Math/floor (/ max-climber-capacity (+ total-climbers 1))))
          reduced-capacity   (reduced-climber-capacity total-climbers max-climber-capacity optimal-breakpoint)]
      (int (+ optimal-breakpoint (fast-optimal-ascent-length (dec total-climbers) reduced-capacity))))))

(comment
  (pprint "=========================")
  (def total-climbers 12)
  (def capacity 51)
  (fast-optimal-ascent-length total-climbers capacity)
  (-> (optimal-ascent total-climbers capacity) ascent-length))
