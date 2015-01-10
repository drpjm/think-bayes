(ns think-bayes.estimation
  (:require [think-bayes.core :refer :all]))

; Example of making collection of integers into strings. Old distribution function needed only strings.
(map (fn [n] (java.lang.Integer/toString n)) [4 5])

; Dice problem.
; Hypotheses: Five dice with different number of faces.
; Data: integers from 1 -> 20.
(def dice-distribution (distribution [4 6 8 12 20]))

; Likelihood: 0 if the data is larger than the hypothesis, otherwise, likelihood of 1/hypothesis.
(defn dice-like-fn [h d]
  (if (< h d)
    0
    (/ 1.0 h)))

(defn roll [ns]
  "Determines the probability of the type of die given a series of rolls."
  (loop [curr-ns ns
         curr-dist dice-distribution]
    (if (empty? curr-ns)
      curr-dist
      (recur (rest curr-ns) (update-prob curr-dist dice-like-fn (first curr-ns))))))