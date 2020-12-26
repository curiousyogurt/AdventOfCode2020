;;;;
;;;; Advent of Code 2020: Day 2, Part a
;;;; https://adventofcode.com/2020/day/2
;;;;
;;;; Password Philosophy
;;;;
(ns day2a.ns
  (:require [clojure.string :as str]))

;;;
;;; Load in the data for the problem.
;;;
(def input-data
  (->> "resources/day2.txt"
    slurp str/split-lines))

;;;
;;; Break data into :minimum :maximum :character :password
;;;
(defn groom-data
  [data]
  (map #(hash-map :minimum (Integer/parseInt (re-find #"^\d+" %))
                  :maximum (Integer/parseInt (re-find #"(?<=-)\d*" %))
                  :character (nth (re-find #"\w?(?=:)" %) 0)
                  :password (re-find #"(?<=: )\w*" %))
       data))

;;;
;;; Destructure entry, then filter based on character and count.
;;; Return true if count between minimum and maximum.
;;;
(defn validPassword?
  [{:keys [minimum maximum character password]}]
  (let [times (count (filter #(= character %) password))]
    (<= minimum times maximum)))
             
;;;
;;; Filter according to validPassword?, then count results
;;;
(defn solve
  [data]
  (count (filter validPassword? data)))

;;;
;;; Kick off computation
;;;
(solve (groom-data input-data))
