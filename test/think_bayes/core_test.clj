(ns think-bayes.core-test
  (:require [clojure.test :refer :all]
            [think-bayes.core :refer :all]))

; Cookie problem example
; Bowl 1 has 30 vanilla, 10 chocolate cookies; Bowl 2 has 20 vanilla, 20 chocolate
(defn cookie-problem []
  (let [bowl-dist {:bowl1 1 :bowl2 1} ; one of each bowl
       norm-bowl-dist (normalize bowl-dist)
       posterior-dist (normalize (mult 
                                   (mult norm-bowl-dist :bowl1 0.75) ; likelihood of puling vanilla from bowl 1
                                   :bowl2 0.5))] ; likelihood of pulling vanilla from bowl 2
   (println "Posterior distribution = " posterior-dist)))

; Cookie problem example using the bayesian framework
(defn cookie-problem2 []
  (let [test-dist (distribution ["bowl1" "bowl2"])
        mixes {:bowl2 {:choc 0.5, :van 0.5} :bowl1 {:choc 0.25, :van 0.75}}
        like-fn (fn [h d]
                  (d (h mixes)))]
    (println "Posterior distribution = " (update-prob test-dist like-fn :van))))

; Implementation of the Monty-Hall problem in Section 2.5.
(defn monty-hall-problem [door-choice]
  (let [monty-dist (distribution ["A" "B" "C"])
        like-fn (fn [h d]
                  (cond
                    (= h d) 0
                    (= h :A) 0.5
                    :else 1))]
    (println "Posterior distribution = " (update-prob monty-dist like-fn door-choice))))

(defn remove-cookie [bowl-mix bowl-key flavor]
  "Removes a cookie with flavor from the bowl with key bowl-key."
  (swap! bowl-mix (fn [x] (update-in x [bowl-key flavor] dec))))

(defn cookie-ratio-map [cookie-mix-map]
  "Function that takes a cookie-mix-map that has counts of cookies and turns them into ratios."
  (loop [bowls (keys cookie-mix-map)
         new-map {}]
    (if (empty? bowls)
      new-map
      (let [curr-hypo-map ((first bowls) cookie-mix-map)
            total (apply + (vals curr-hypo-map))
            curr-ratio-map (assoc {}
                                  :van (float (/ (:van curr-hypo-map) total)) 
                                  :choc (float (/ (:choc curr-hypo-map) total)))]
        (recur (rest bowls) (assoc new-map (first bowls) curr-ratio-map))))))

; Exercise 2.1 - Cookie problem *without* replacement.
(defn cookie-problem-noreplace [ds]
  "Launches the cookire problem with no replacement based on a coll of data, ds."
  (let [test-dist (distribution ["bowl1" "bowl2"])
        mixes [(atom {:bowl2 {:choc 20, :van 20} :bowl1 {:choc 10, :van 30}}) ; bowl1
               (atom {:bowl2 {:choc 20, :van 20}, :bowl1 {:choc 10, :van 30}})] ; bowl2
        like-fn (fn [h d]
                  (if (= h :bowl1)
                    (let [like-b1 (d (h (cookie-ratio-map @(first mixes))))]
                      (remove-cookie (first mixes) :bowl1 d)
                      like-b1)
                    (let [like-b2 (d (h (cookie-ratio-map @(second mixes))))] 
                      (remove-cookie (second mixes) :bowl2 d)
                      like-b2)))]
    (loop [curr-ds ds
           curr-posterior test-dist]
      (if (empty? curr-ds)
        curr-posterior
        (recur (rest curr-ds) (update-prob curr-posterior like-fn (first ds))))
      )))