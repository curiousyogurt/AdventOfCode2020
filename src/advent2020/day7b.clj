;;;;
;;;; Advent of Code 2020: Day 7, Part b
;;;; https://adventofcode.com/2020/day/7
;;;;
;;;; Handy Haversacks
;;;;
(ns day7b.ns
  (:require [clojure.string :as str]))

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
    {antecedent (str/split consequent #" bag(s)?(\, |.)")}))

;;;
;;; Use maps for parsed data so that we can look up the consequent with `(get
;;; <dataset name> <colour>).
;;;
(def parsed-input-data (apply merge (map #(into {} (parse-items %)) input-data)))
(def parsed-sample-data (apply merge (map #(into {} (parse-items %)) sample-data)))

;;;
;;; Convert a consequent into an integer together with a description.
;;; For example, ["1 bright white"] ==> [1 "bright white"].  Scan specifically
;;; for "no other" as the consequent.  In such a case, return [0 nil].  Any
;;; search of the form (get <dataset name> <colour>) will return [].
;;; 
(defn parse-consequent
  [consequent]
  (if (= "no other" consequent)
    [0 nil]
    (let [[_ number description]
          (re-matches #"(\d+) (\w+ \w+)" consequent)]
      [(Integer/parseInt number) description])))

;;;
;;; Takes an antecedent and returns the consequent with all elements parsed.
;;;
(defn consequents
  [data antecedent]
  (mapv parse-consequent (get data antecedent)))

;;;
;;; Count the bags in a given bag.  Do this by reduce, taking as the argument
;;; some starting value (k), then a collection of consequents, deconstructed
;;; so that we have n (the number of bags) and the colour.  Then, use reduce
;;; to add to k the number of bags in the current element of the collection,
;;; and then mulitply by n the number of bags that go in the colour.  Do this
;;; recursively to solve the problem.
;;;
(defn bag-count
  [data antecedent]
  (let [consequent (consequents data antecedent)]
    (reduce (fn [k [n colour]]
              (+ k n
                 (* n (bag-count data colour)))) 0 consequent)))

;;;
;;; Given rules (data) and a target as a string, count up all the containers.
;;;
(defn solve
  [data target]
  (bag-count data target))

;;;
;;; Kick off computation
;;;
(solve parsed-sample-data "shiny gold") ; ==> 32
(solve parsed-input-data "shiny gold")  ; ==> 58175
