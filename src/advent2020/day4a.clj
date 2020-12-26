;;;;
;;;; Advent of Code 2020: Day 5, Part b
;;;; https://adventofcode.com/2020/day/4
;;;;
;;;; Password Philosophy
;;;;
(ns day4b.ns
  (:require [clojure.string :as str]
            [clojure.walk :as wlk]))

;;;
;;; Load in the data for the problem.
;;;
(def input-data
  (->> "resources/day5.txt"
    slurp str/split-lines
    (mapv read-string)))

;;;
;;; Convert to binary
;;;
(defn FB->binary
  [str
   (let [f->0 (str/replace str  "F" "0")
         b->1 (str/replace f->0 "B" "1")
         l->0 (str/replace b->1 "L" "0")
         r->1 (str/replace l->0 "R" "1")
         bins (re-matches #"(\d{7})(\d{3})" r->1)]
     [(second bins) (last bins)])])
   
(FB->binary "FFFBBBBLRL")


(let [F->0 (str/replace "FFFFBBBLRL"  "F" "0")
      B->1 (str/replace F->0 "B" "1")
      L->0 (str/replace B->1 "L" "0")
      R->1 (str/replace L->0 "R" "1")
      bins (re-matches #"(\d{7})(\d{3})" R->1)]
  [(second bins) (last bins)])
         
(let [F->0 (str/replace "FF" "F" "0")]
  F->0)


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
;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;; hgt (Height) - a number followed by either cm or in:
;; If cm, the number must be at least 150 and at most 193.
;; If in, the number must be at least 59 and at most 76.
;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;; pid (Passport ID) - a nine-digit number, including leading zeroes.
;; cid (Country ID) - ignored, missing or not.
;;
(defn validPassport?
  [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and byr iyr eyr hgt hcl ecl pid
       (<= 1920 (Integer/parseInt byr) 2002)
       (<= 2010 (Integer/parseInt iyr) 2020)
       (<= 2020 (Integer/parseInt eyr) 2030)
       (let [[_ magnitude unit] (re-matches #"(\d+)(cm|in)" hgt)]
        (cond (= unit "cm") (<= 150 (Integer/parseInt magnitude) 193)
              (= unit "in") (<= 59 (Integer/parseInt magnitude) 76)
              :else false))
       (re-matches #"#[a-f0-9]{6}" hcl)
       (re-matches #"amb|blu|brn|gry|grn|hzl|oth" ecl)
       (re-matches #"\d{9}" pid)))

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
