;;;;
;;;; Advent of Code 2020: Day 3, Part a
;;;; https://adventofcode.com/2020/day/3
;;;;
;;;; Toboggan Trajectory
;;;;
(ns day3a.ns
  (:require [clojure.string :as str]))

;;;
;;; Load in the data for the problem.
;;;
(def input-data
  (->> "resources/day3.txt"
    slurp str/split-lines))

;;;
;;; Determine for a given line whether there is a tree at position x.
;;; Wrap around as necessary.
;;;
(defn tree?
  [line x]
  (= \# (nth line 
             (mod x (count line))))) ; Wrap around

(defn toboggan
  [data skip]
  (let [columns (iterate #(+ skip %) 0)
        targets (map vector data columns)]
    (reduce (fn [results target]
              (into results
                    (if (tree? (first target) (second target))
                      [(first target)] ; if tree, then store
                      nil)))           ; otherwise, nil
            []
            targets)))

;;;
;;; Toboggan down the slope and tally the results
;;;
(defn solve
  [data skip]
  (count (toboggan data skip)))

;;;
;;; Kick off computation
;;;
(solve input-data 3)
