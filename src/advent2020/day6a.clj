;;;;
;;;; Advent of Code 2020: Day 6, Part a
;;;; https://adventofcode.com/2020/day/6
;;;;
;;;; Binary Boarding
;;;;
(ns day6a.ns
  (:require [clojure.string :as str]))

;;;
;;; Load in the data for the problem
;;;
(def input-data
  (str/split (slurp "resources/day6.txt") #"\n\n"))

;;;
;;; Load each group into sets (automatically eliminating any duplicates), then
;;; call disj \newline to get rid of \n between individual passengers.  Count
;;; up the number of elements.
;;;
(defn count-yeses
  [data]
  (-> data
     set
     (disj \newline)
     count))

;;;
;;; Apple count-yeses to every element of data, the add up the results.
;;;
(defn solve
  [data]
  (->> data
       (map count-yeses)
       (reduce +)))

;;;
;;; Kick off computation
;;;
(solve input-data)
