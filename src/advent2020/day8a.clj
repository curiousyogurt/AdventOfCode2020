() ;;;;
;;;; Advent of Code 2020: Day 8, Part a
;;;; https://adventofcode.com/2020/day/8
;;;;
;;;; Handheld Halting
;;;;)
(ns day8a.ns
  (:require [clojure.string :as str]))

;;;
;;; Load in the data for the problem
;;;
(def input-data
  (str/split (slurp "resources/day8.txt") #"\n"))

;;;
;;; Sample data from the qusetion; easier to handle than all the data.
;;;
(def sample-data (str/split "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6" #"\n"))

;;;
;;; For each instruction as a string, parse into vector of form <operation>
;;; <argument>, where <operation> i a three-letter string, and <argument> is
;;; an integer.
;;;
(defn parse-instruction
  [instruction]
  (let [[_ operation argument _]
        (re-matches #"(\w+) ((\+|\-)\d+)" instruction)]
    [operation (Integer/parseInt argument)]))

;;;
;;; Define parsed sample and input data.
;;;
(def parsed-sample-data (mapv parse-instruction sample-data))
(def parsed-input-data (mapv parse-instruction input-data))

;;;
;;; Use the vector <executed> to keep track of which instructions we have
;;; already executed.  Check whether the current instruction has been
;;; executed by comparing <executed> against <pointer>.
;;;
(defn not-executed-before? [executed pointer]
  (nil? (some #(= pointer %) executed)))

;;;
;;; Since execute and opertaion-* call each other, let operation-* functions
;;; know we intend to define execute.
;;;
(declare execute)

;;;
;;; Nop: ignore, and increase the pointer by 1.
;;;
(defn operation-nop
  [_ accumulator pointer executed data]
  (let [new-pointer (inc pointer)]
    (if (not-executed-before? executed new-pointer)
      (execute accumulator new-pointer (conj executed new-pointer) data)
      accumulator)))

;;;
;;; Acc: increase/decrease the accumulator, and increase teh pointer by 1.
;;;
(defn operation-acc
  [argument accumulator pointer executed data]
  (let [new-pointer (inc pointer)]
    (if (not-executed-before? executed new-pointer)
      (execute (+ accumulator argument) new-pointer (conj executed new-pointer) data)
      ( accumulator))))

;;;
;;; Jmp: execute the jump.
;;;
(defn operation-jmp
  [argument accumulator pointer executed data]
  (let [new-pointer (+ pointer argument)]
    (if (not-executed-before? executed new-pointer)
      (execute accumulator new-pointer (conj executed new-pointer) data)
      accumulator)))

;;;
;;; For the instruction an pointer, figure out which operation we need to
;;; call, atnd call it.
;;;
(defn execute
  [accumulator pointer executed data]
  (let [instruction (nth data pointer)
        [operation argument] instruction]
    ((condp = operation
       "nop" operation-nop
       "acc" operation-acc
       "jmp" operation-jmp)
     argument accumulator pointer executed data)))

;;;
;;; Execute the program: accumulator 0, pointer 0, executed an empty vector,
;;; and data being the program.
;;;
(defn solve
  [data]
  (execute 0 0 [] data))

;;;
;;; Kick off computation
;;;
(solve parsed-sample-data) ; => 5
(solve parsed-input-data)  ; => 1941
