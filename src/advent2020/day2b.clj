;;;;
;;;; Advent of Code 2020: Day 2, Part b
;;;; https://adventofcode.com/2020/day/2
;;;;
;;;; Password Philosophy
;;;;
(ns day2b.ns
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
  (map #(hash-map :pos1 (Integer/parseInt (re-find #"^\d+" %))
                  :pos2 (Integer/parseInt (re-find #"(?<=-)\d*" %))
                  :character (nth (re-find #"\w?(?=:)" %) 0)
                  :password (re-find #"(?<=: )\w*" %))
       data))

;;;
;;; Destructure entry, then use exclusive or (not=)
;;;
(defn validPassword?
  [{:keys [pos1 pos2 character password]}]
  (not= (= (nth password (dec pos1)) character)
        (= (nth password (dec pos2)) character)))

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
