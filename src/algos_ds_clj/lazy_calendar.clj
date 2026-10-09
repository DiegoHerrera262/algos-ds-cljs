(ns algos-ds-clj.lazy-calendar
  (:import (java.time LocalDate)))

(defn str->local-date [date-str]
  (LocalDate/parse date-str))

(defn local-date->str [date]
  (.toString date))

(defn add-days [days-to-add date]
  (.plusDays date days-to-add))

(defn time-seq [from-date]
  (->> from-date
       (iterate (partial add-days 1))))

(defn day-of-week [date]
  (let [day-str (-> date (.getDayOfWeek) (.toString))]
    (->> day-str
         (get {"MONDAY"    0
               "TUESDAY"   1
               "WEDNESDAY" 2
               "THURSDAY"  3
               "FRIDAY"    4
               "SATURDAY"  5
               "SUNDAY"    6}))))

(defn prior-sunday [date]
  (let [day-number (day-of-week date)]
    (if-not (= day-number 6)
      (-> day-number
          inc
          -
          (add-days date))
      date)))

(defn calendar-seq [date]
  (->> date
       time-seq
       (partition-by prior-sunday)))

(defn calendar->str-seq [cal]
  (->> cal
       (map #(map local-date->str %))))

(comment
  (->> "2024-12-30"
       str->local-date
       calendar-seq
       calendar->str-seq
       (take 6))
  (->> "2025-04-22"
       str->local-date
       day-of-week)
  (->> "2025-01-03"
       str->local-date
       time-seq
       (take 4)
       (map local-date->str))
  (->> "2025-01-03"
       str->local-date
       (add-days -4)
       local-date->str))
