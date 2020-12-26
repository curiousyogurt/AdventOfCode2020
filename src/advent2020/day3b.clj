;;;;
;;;; Advent of Code 2020: Day 3, Part b
;;;; https://adventofcode.com/2020/day/3
;;;;
;;;; Toboggan Trajectory
;;;;
(ns day3b.ns
  (:require [clojure.string :as str]))

;;;
;;; Load in the data for the problem.
;;;
(def input-data
  (->> "resources/day3.txt"
    slurp str/split-lines))

;;;
;;; Determine for a given line whether there is a tree at position x. Wrap
;;; around as necessary.
;;;
(defn tree?
  [[line x]] ; destructure vector input; specifically done for filter
  (= \# (nth line 
             (mod x (count line))))) ; Wrap around

;;;
;;; A much more efficient version of toboggan than 2a.  The true/false natrue
;;; of the results mean 'filter' is natural.
;;;
(defn toboggan
  [data skip]
  (let [columns (iterate #(+ skip %) 0)
        targets (map vector data columns)]
    (filter tree? targets)))

;;;
;;; Toboggan down the slope and tally the results
;;;
(defn solve
  [data]
  (* (count (toboggan data 1))
     (count (toboggan data 3))
     (count (toboggan data 5))
     (count (toboggan data 7))
     (count (toboggan (take-nth 2 data) 1)))) ;; take-nth taes every second entry
     
;;;
;;; Kick off computation
;;;
(solve input-data)
