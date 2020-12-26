;;;;
;;;; Advent of Code 2020: Day 6, Part b
;;;; https://adventofcode.com/2020/day/6
;;;;
;;;; Binary Boarding
;;;;
(ns day6b.ns
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

;;;
;;; Load in the data for the problem
;;;
(def input-data
  (str/split (slurp "resources/day6.txt") #"\n\n"))

;;;
;;; Load each group into sets (automatically eliminating any duplicates), then
;;; split along newlines, then turn everything into sets and pick out the
;;; intersection.  Then count up the number of elements.
;;;
(defn count-joint-yeses
  [data]
  (as-> data v
   (str/split v #"\n")
   (map set v)
   (apply set/intersection v)
   (count v)))

;;;
;;; Map count-yeses to every element of data, the add up the results.
;;;
(defn solve
  [data]
  (->> data
       (map count-joint-yeses)
       (reduce +)))

;;;
;;; Kick off computation
;;;
(solve input-data)
