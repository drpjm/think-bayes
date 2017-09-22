(ns think-bayes.estimation
  (:require [think-bayes.core :refer :all]
            [think-bayes.viz :refer :all]
            [think-bayes.util :refer :all]))

; 3.1 Dice problem!
; Hypotheses: Five dice with different number of faces.
; Data: integers from 1 -> 20.

(defn dice-like-fn [h d]
  "Likelihood: 0 if the data is larger than the hypothesis, otherwise, likelihood of 1/hypothesis." 
  (if (< h d)
    0
    (/ 1.0 h)))

(defn roll [ns dice-dist]
  "Determines the probability of the type of die given a series of rolls and dice distribution."
  (loop [curr-ns ns
         curr-dist dice-dist]
    (if (empty? curr-ns)
      curr-dist
      (recur (rest curr-ns) (update-prob curr-dist dice-like-fn (first curr-ns))))))

; Run with [4 6 8 12 20] for the types of dice
(defn run-dice-problem [dice data]
  (let [dice-distribution (uniform-distribution dice)]
    (print (roll data dice-distribution))))

; 3.2 Locomotive problem!
; Setup: railroad numbers its locomotives as 1 -> N. You see a locomotive with a given number, n. Estimate how
; many locomotives the company has.
; Q: What did we know about N before we saw the data? For any N, what is the likelihood of seeing the data, n?
; A: PRIOR, LIKELIHOOD
; If there is one company, all equally likely. Thus, the likelihood function looks just like the dice...

(defn locomotive-like-fn [h d]
  (if (< h d)
    0
    (/ 1.0 h)))

(defn run-locomotive-problem1 [N]
  (let [init-dist (uniform-distribution (range 1 (+ 1 N)))
        after60 (update-prob init-dist locomotive-like-fn 60) ; See engine 60
        ]
    (println "Check probability against the graph in the book:")
    (println "Prob(60 trains) = " (get after60 60))
    (println "Prob(200 trains) = " (get after60 200))
    ; Right now, 60 is the most likely (of course). But is it the right goal? 
    ; Let's compute the mean of the posterior:
    (println "Posterior mean = " (mean after60))))

; The mean of the posterior is useful for minimizing mean square error!

; Original formulation needs more data to come up with anything realistic. We want to run the update-prob
; function on a sequence of "seen" locomotives.
(defn estimate-num-locomotives [ns dist]
  (loop [curr-ns ns
         curr-dist dist]
    (if (empty? curr-ns)
      curr-dist
      (recur (rest curr-ns) (update-prob curr-dist locomotive-like-fn (first curr-ns))))))

(let [smaller-dist (uniform-distribution (range 1 301))
      med-dist (uniform-distribution (range 1 1001))
      large-dist (uniform-distribution (range 1 2001))]
  (println "Uniform distributions: Posterior means for seeing trains 60, 30, 90:" 
           "\nsmall = " (mean (estimate-num-locomotives [60 30 90] smaller-dist))
           "\nmed = " (mean (estimate-num-locomotives [60 30 90] med-dist))
           "\nlarge = " (mean (estimate-num-locomotives [60 30 90] large-dist))))

; Utilizing a power distribution, rather than uniform, we can create a less sensitive prior.
; The power distribution is based on the notion that smaller things are more likely than larger things.
(let [smaller-dist (power-distribution (range 1 501) 1.0)
      med-dist (power-distribution (range 1 1001) 1.0)
      large-dist (power-distribution (range 1 2001) 1.0)]
  (println "Power distributions: Posterior means for seeing trains 60, 30, 90:" 
           "\nsmall = " (mean (estimate-num-locomotives [60 30 90] smaller-dist))
           "\nmed = " (mean (estimate-num-locomotives [60 30 90] med-dist))
           "\nlarge = " (mean (estimate-num-locomotives [60 30 90] large-dist))))

; To generate the estimate of a data point, we typically compute an interval where there is a 90%
; chance that value falls within the interval.
(let [smaller-dist (power-distribution (range 1 501) 1.0)
      estimated-dist (estimate-num-locomotives [60 30 90] smaller-dist)]
  (println "Percentile boundaries for small train distribution:\n5th = "
           (percentile estimated-dist 5) "\n95th = "
           (percentile estimated-dist 95)))

; CDFs allow you to compute percentiles more easily. CDFs and PMFs can be converted into each other.
(let [smaller-dist (power-distribution (range 1 51) 1.0)]
  (println "Probability of selecting 20 or less: " 
           (prob-of (cdf-from-pmf smaller-dist) 20)))

; Euro coin problem
; The hypothesis is the probability that a euro coin lands face up, x% where x on [0,100].
(defn euro-likelihood [hypo data]
  ; Assumes hypo is a number
  (if (= "H" data)
    (/ hypo 100.0)
    (- 1 (/ hypo 100.0))))

; General estimation function given data, distribution, and the likelihood function.
(defn estimate-with-data [dist like-fn data]
  (loop [curr-data data
         curr-dist dist]
    (if (empty? curr-data)
      curr-dist
      (recur (rest curr-data) (update-prob curr-dist like-fn (first curr-data))))))

; Use the above functions to create the distribution in figure 4.1 and this data
(def flip-data
  (into [] (concat (repeat 140 "H") (repeat 110 "T")) ))

(defn run-euro-flip [data]
  (let [heads-dist (uniform-distribution (range 0 101))
        new-dist (estimate-with-data heads-dist euro-likelihood flip-data)]
    (plot-distribution new-dist)
    new-dist))
; Note that the probability in this estimated distribution is meaningless on its own!


