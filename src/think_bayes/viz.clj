(ns think-bayes.viz
  (:require [incanter.core :as i]
            [incanter.charts :as charts]))

(defn plot-distribution [dist]
  (i/view (charts/xy-plot (keys dist) (vals dist) :x-label "Item" :y-label "Probability")))
