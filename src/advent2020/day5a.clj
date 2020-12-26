;;;;
;;;; Advent of Code 2020: Day 5, Part a
;;;; https://adventofcode.com/2020/day/5
;;;;
;;;; Binary Boarding
;;;;
(ns day5a.ns
  (:require [clojure.string :as str]))

;;;
;;; Load in the data for the problem.
;;;
(def input-data
  (str/split (slurp "resources/day5.txt") #"\n"))

;;;
;;; Convert letters in the boarding pass to binary according to the following
;;; scheme: F->0, B->1 L->0, R->1.  Store as vectors of pairs of elements.
;;;
(defn pass->binpass
  [str]
  (let [fl->0 (str/replace str   #"F|L" "0")
        br->1 (str/replace fl->0 #"B|R" "1")
        bins (re-matches #"(\d{7})(\d{3})" br->1)]
    [(second bins) (last bins)]))


;;;
;;; Vector of pairs of string binaries to integers.  Perform seatId
;;; calcualtion.
;;;
(defn seatId
  [binpass]
  (let [row (Integer/parseInt (first binpass) 2)
        col (Integer/parseInt (second binpass) 2)]
   (+ (* row 8) col)))

;;;
;;; Check for valid passports
;;;
(defn solve
  [data]
  (->> data
       (mapv pass->binpass)
       (mapv seatId)
       sort
       last))
 

;;;
;;; Kick off computation
;;;
(solve input-data)
