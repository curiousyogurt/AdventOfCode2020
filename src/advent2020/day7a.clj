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
    {antecedent (str/split consequent #" bag(s)?(\, |.)")}))

;;;
;;; Use maps for parsed data so that we can look up the consequent with `(get
;;; <dataset name> <colour>).
;;;
(def parsed-input-data (apply merge (map #(into {} (parse-items %)) input-data)))
(def parsed-sample-data (apply merge (map #(into {} (parse-items %)) sample-data)))

;;;
;;; Predicate that returns true iff the consequent (after "contains") matches
;;; a target description.  Target is assumed to be a string. For example,
;;; "shiny gold".
;;;
(defn contains-bag?
  [rule target]
  (->> rule
       last
       (filter #(str/includes? % target))
       empty?
       not))

;;;
;;; Given a set of rules (data) and a target as a string (e.g., "shiny gold")
;;; pick out just those rules that match.
;;;
(defn find-rules
  [data target]
  (into {} (filter #(contains-bag? % target) data)))

;;;
;;; Given rules (data) and a target as a string (e.g., "shiny gold"), build
;;; up a results vector by finding all those rules that have the target as
;;; a consequent.  Move (first remaining) to results; and add any new bags
;;; (as antecedents) to remaining.
;;;
(defn find-all-containers
  [data target]
  (loop
    [results []
     remaining target]
    (if (empty? remaining)
      (rest (set results)) ; Remove the first element (=target)
      (let [result (find-rules data (first remaining))]
        (recur (conj results (first remaining))
               (into (rest remaining) (mapv first result)))))))

;;;
;;; A revised version of find-all-containers, specifically designed to use
;;; straightup recursion (no loops), in order to have a more straightforward
;;; presentation of the function.  The difference between the original version
;;; and -revised, aside form being shorter and calling itself directly, is
;;; that by calling `(map #(find-rules data %))`, we are able to deal with
;;; multiple targets as opposed to just one at a time.
;;;
(defn find-all-containers-revised
  [data targets]
  (let [results (->> targets
                     (into {} (map #(find-rules data %)))
                     (map first))] 
    (if-not (empty? results)
      (into results (find-all-containers-revised data results)))))

;;;
;;; Given rules (data) and a target as a string, count up all the containers.
;;;
(defn solve
  [data target]
  (count (set (find-all-containers-revised data [target]))))

;;;
;;; Kick off computation
;;;
(solve parsed-sample-data "shiny gold") ; ==> 4
(solve parsed-input-data "shiny gold")  ; ==> 139
