;;;;
;;;; Advent of Code 2020: Day 1, Part a
;;;; https://adventofcode.com/2020/day/1
;;;;
;;;; Report Repair
;;;;
(ns day1a.ns
  (:require [clojure.string :as str]))

;;;
;;; Load in the data for the problem.
;;;
(def input-data
  (->> "resources/day1.txt"
    slurp str/split-lines
    (mapv read-string)))

;;;
;;; Use a list comprehension: {a b | a + b = 2020, and a < b}
;;;
(defn addup
  [numbers]
  (for [a numbers
        b numbers
        :when (< a b)
        :when (= 2020 (+ a b))]
    [a b]))

;;;
;;; Compute solution and multiply results
;;;
(defn solve
  [data]
  (let [solution (first (addup data))]
    (* (first solution) (second solution))))
       
;;;
;;; Kick off computaiton
;;;
(solve input-data)
