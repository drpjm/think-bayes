(ns think-bayes.util)

; Example of making collection of integers into strings. Old distribution function needed only strings.
(defn int-list-to-strings [int-list]
  (map (fn [n] (java.lang.Integer/toString n)) int-list))
