;;;;
;;;; Advent of Code 2020: Day 7, Part a
;;;; https://adventofcode.com/2020/day/7
;;;;
;;;; Handy Haversacks
;;;;
(ns day7a.ns
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

;;;
;;; Load in the data for the problem
;;;
(def input-data
  (str/split (slurp "resources/day7.txt") #"\n"))

;;;
;;; Sample data from question; easier to handle than all the data.
;;;
(def sample-data (str/split "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags." #"\n"))

;;;
;;; Investigate data properties.  Return true iff all antecedents (the bag
;;; description before "bags contains") are unique — just to make sure.
;;;
(defn unique-antecedents?
  [data]
  (= (count data)
     (->> data
          (map parse-items)
          (map first)
          set
          count)))

(unique-antecedents? input-data)  ; => true
(unique-antecedents? sample-data) ; => true

;;;
;;; Parse single-string statements of each rule.  So:
;;;
;;; "light red bags contain 1 bright white bag, 2 muted yellow bags."
;;; ->
;;; ["light red" ["1 bright white" "2 muted yellow"]]
;;;
;;; Rule terminology: antecedent - everything before "bags contain"
;;;                   consequent - everything after "contain"
;;;
(defn parse-items
  [item]
  (let [[_ antecedent consequent]
        ;; Match before and after "bags contain", removing spacing.
        (re-matches #"(\w+ \w+) bags contain (.+)" item)]
    ;; For consequent (after "bags contain"), split along commas.
    [antecedent (str/split consequent #" bag(s)?(\, |.)")]))

(def parsed-input-data (map parse-items input-data))
(def parsed-sample-data (map parse-items sample-data))

;;;
;;; Predicate that returns true iff the consequent (after "contains") matches
;;; a target description.  Target is assumed to be a string. For example,
;;; "shiny gold".
;;;
(defn contains-bag?
  [rule target]
  (->> rule
       last
       (filterv #(str/includes? % target))
       empty?
       not))

;;;
;;; Given a set of rules (data) and a target as a string (e.g., "shiny gold")
;;; pick out just those rules that match.
;;;
(defn find-bags
  [data target]
  (filterv #(contains-bag? % target) data))

;;;
;;; Given rules (data) and a target as a string (e.g., "shiny gold"), build
;;; up a results vector by finding all those rules that have the target as
;;; a consequent.  Move (first remaining) to results; and add any new bags
;;; (as antecedents) to remainincg.
;;;
(defn find-all-containers
  [data target]
  (loop
    [results []
     remaining target]
    (if (empty? remaining)
      (rest (set results)) ; Remove the first element (=target)
      (let [result (find-bags data (first remaining))]
        (recur (conj results (first remaining))
               (into (rest remaining) (mapv first result)))))))

;;;
;;; Given rules (data) and a target as a string, count up all the containers.
;;;
(defn solve
  [data target]
  (count (find-all-containers data [target])))

;;;
;;; Kick off computation
;;;
(solve parsed-sample-data "shiny gold") ; ==> 4
(solve parsed-input-data "shiny gold")  ; ==> 139
