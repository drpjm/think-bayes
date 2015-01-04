(ns think-bayes.core)

(defn distribution []
  {})

(defn set-prob [dist item pr]
  (assoc dist item pr))

(defn prob [dist item]
  (item dist))

(defn increment [dist item]
  (if (nil? (item dist))
    (conj dist {item 0})
    (assoc dist item (inc (item dist)))))

(defn mult [dist item likelihood]
  (if (nil? (item dist))
    dist
    (assoc dist item (* (item dist) likelihood))))

(defn normalize [dist]
  "Normalizes the dist for computing the probability of a item from the dist."
  (let [denom (apply + (vals dist))]
    (reduce-kv (fn [m k v] (assoc m k (/ v denom))) {} dist)))
