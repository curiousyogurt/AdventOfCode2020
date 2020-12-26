;;;;
;;;; Advent of Code 2020: Day 4, Part a
;;;; https://adventofcode.com/2020/day/4
;;;;
;;;; Password Philosophy
;;;;
(ns day4a.ns
  (:require [clojure.string :as str]
            [clojure.walk :as wlk]))

;;;
;;; Load in the data for the problem.
;;;
(defn groom-data
  [data]
  ;; For as-> sequence, here are the folliwng stages:
  ;; Original sample string                                 : "a:1 b:2 c:3"
  (as-> data v
    ;; Split along spaces                                   : ["a:1" "b:2" "c:3"]
    (str/split v #"\s")
    ;; Split along colons                                   : (["a" "1"] ["b" "2"] ["c" "3"])
    (map #(str/split % #":") v)
    ;: Copy into a hash                                     : {"a" "1", "b" "2", "c" "3"}
    (into {} v)
    ;; Convert into keywords to be ideomatic                : {:a "1", :b "2", :c "3"}
    (wlk/keywordize-keys v)))

;;
;; Get intoput data from file.  Split along double \n (blank lines in between records).
;; Apply groom-data to every entry.
;;
(def input-data
  (as-> (slurp "resources/day4.txt") v
    (str/split v #"\n\n")
    (map groom-data v)))

;;
;; Destructure according to keys, and then evaluate by and (nil will make
;; passports with missing fields false.
;;
(defn validPassport?
  [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and byr iyr eyr hgt hcl ecl pid))
  
;;;
;;; Check for valid passports
;;;
(defn solve
  [data]
  (count (filter validPassport? data)))

;;;
;;; Kick off computation
;;;
(solve input-data)
