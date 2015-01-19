(ns think-bayes.estimation
  (:require [think-bayes.core :refer :all]
            [think-bayes.viz :refer :all]))

; Example of making collection of integers into strings. Old distribution function needed only strings.
(map (fn [n] (java.lang.Integer/toString n)) (range 1 10 1))

; 3.1 Dice problem!
; Hypotheses: Five dice with different number of faces.
; Data: integers from 1 -> 20.
(def dice-distribution (uniform-distribution [4 6 8 12 20]))

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

; 3.2 Locomotive problem!
; Setup: railroad numbers its locomotives as 1 -> N. You see a locomotive with a given number, n. Estimate how
; many locomotives the company has.
; Q: What did we know about N before we saw the data? For any N, what is the likelihood of seeing the data, n?
; A: PRIOR, LIKELIHOOD
(def locomotive-distribution (uniform-distribution (range 1 1001)))

; If there is one company, all equally likely. Thus, the likelihood function looks just like the dice...
(defn locomotive-like-fn [h d]
  (if (< h d)
    0
    (/ 1.0 h)))

; Check against graph in the book:
(get (update-prob locomotive-distribution locomotive-like-fn 60) 60)
(get (update-prob locomotive-distribution locomotive-like-fn 60) 180)
(get (update-prob locomotive-distribution locomotive-like-fn 60) 900)

; Right now, 60 is the most likely (of course). But is it the right goal? Let's compute the mean of the posterior:
(mean (update-prob locomotive-distribution locomotive-like-fn 60))
; The mean of the posterior is useful for minimizing mean square error!

; 3.3 Locomotive redux!
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

; 3.4 Power distribution
; Utilizing a power distribution, rather than uniform, we can create a less sensitive prior.
; The power distribution is based on the notion that smaller things are more likely than larger things.
(let [smaller-dist (power-distribution (range 1 501) 1.0)
         med-dist (power-distribution (range 1 1001) 1.0)
         large-dist (power-distribution (range 1 2001) 1.0)]
     (println "Power distributions: Posterior means for seeing trains 60, 30, 90:" 
              "\nsmall = " (mean (estimate-num-locomotives [60 30 90] smaller-dist))
              "\nmed = " (mean (estimate-num-locomotives [60 30 90] med-dist))
              "\nlarge = " (mean (estimate-num-locomotives [60 30 90] large-dist))))

; 3.5 Credible intervals
; To generate the estimate of a data point, we typically compute an interval where there is a 90%
; chance that value falls within the interval.
(let [smaller-dist (power-distribution (range 1 501) 1.0)
      estimated-dist (estimate-num-locomotives [60 30 90] smaller-dist)]
  (println "Percentile boundaries for small train distribution:\n5th = "
           (percentile estimated-dist 5) "\n95th = "
           (percentile estimated-dist 95)))

; 3.6 Cumulative distribution functions (cdf)
; CDFs allow you to compute percentiles more easily. CDFs and PMFs can be converted into each other.