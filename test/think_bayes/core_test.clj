(ns think-bayes.core-test
  (:require [clojure.test :refer :all]
            [think-bayes.core :refer :all]))

; Cookie problem example
; Bowl 1 has 30 vanilla, 10 chocolate cookies; Bowl 2 has 20 vanilla, 20 chocolate
(let [bowl-dist {:bowl1 1 :bowl2 1} ; one of each bowl
      norm-bowl-dist (normalize bowl-dist)
      posterior-dist (normalize (mult 
                                  (mult norm-bowl-dist :bowl1 0.75) ; likelihood of puling vanilla from bowl 1
                                  :bowl2 0.5))] ; likelihood of pulling vanilla from bowl 2
  (println "Posterior distribution = " posterior-dist))