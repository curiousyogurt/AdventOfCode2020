;;;;
;;;; Advent of Code 2020: Day 1, Part b
;;;; https://adventofcode.com/2020/day/1
;;;;
;;;; Report Repair
;;;;
(ns day1b.ns
  (:require [clojure.string :as str]))

;;;
;;; Load in the data for the problem.
;;;
(def input-data
  (->> "resources/day1.txt"
    slurp str/split-lines
    (mapv read-string)))

;;;
;;; Use a list comprehension: {a b c | a + b + c = 2020, and a < b < c}
;;;
(defn addup
  [numbers]
  (for [a numbers
        b numbers
        c numbers
        :when (< a b c)
        :when (= 2020 (+ a b c))]
    [a b c]))

;;;
;;; Compute solution and multiply results
;;;
(defn solve
  [data]
  (let [solution (first (addup data))]
    (* (first solution) (second solution) (last solution))))

;;;
;;; Kick off computation
;;;
(solve input-data)
