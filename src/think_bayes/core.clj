(ns think-bayes.core)

(defn normalize [dist]
  "Normalizes the dist for computing the probability of a hypo from the dist."
  (let [denom (apply + (vals dist))]
    (reduce-kv (fn [m k v] (assoc m k (/ v denom))) {} dist)))

(defn distribution [hypos]
  "Creates a distribution with given collection of hypos and each having the same probability."
  (normalize (zipmap (map keyword hypos)
                (repeat (count hypos) 1))))

(defn set-prob [dist hypo pr]
  (assoc dist hypo pr))

(defn prob [dist hypo]
  (hypo dist))

(defn increment [dist hypo]
  (if (nil? (hypo dist))
    (conj dist {hypo 1})
    (assoc dist hypo (inc (hypo dist)))))

(defn mult [dist hypo likelihood]
  "Multiplies the probability of hypo in dist by the given likelihood. Returns the distribution with updated entries."
  (if (nil? (hypo dist))
    dist
    (assoc dist hypo (* (hypo dist) likelihood))))

(defn likelihood [mixes hypo data]
  "Computes the likelihood of the data given the hypo and mixes map."
  (data (hypo mixes)))

(defn update-prob [dist mixes data]
  "Generates a new distribution based on the mixes and given data."
  (let [updated-dist (loop [hypos (keys dist)
                            mod-dist dist]
                       (if (empty? hypos)
                         mod-dist
                         (recur 
                           (rest hypos)
                           (mult mod-dist (first hypos) (likelihood mixes (first hypos) data)))))]
    (normalize updated-dist)))