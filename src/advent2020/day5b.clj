;;;;
;;;; Advent of Code 2020: Day 5, Part b
;;;; https://adventofcode.com/2020/day/5
;;;;
;;;; Binary Boarding
;;;;
(ns day5b.ns
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
(defn valid
  [data]
  (->> data
       (mapv pass->binpass)
       (mapv seatId)
       sort))
 
;;;
;;; Search for the missing seat.  The real star here is '(partition x y)', which
;;; takes a list, such as (89 90 91 92 93 94), and turns it into x-tuples with a
;;; step of y; in our case, ((89 90) (90 91) (91 92) ...).  From here, we remove
;;; all the pairs that are 'inc' apart, and pick out the target seat.
;;;
(d(defn solve
    [data]
    (->> data
         valid
         (partition 2 1)
         (remove #(= (inc (first %)) (second %)))
         first
         first
         inc)))

;;;
;;; Kick off computation
;;;
(solve input-data)
