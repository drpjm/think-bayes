(ns think-bayes.core)

(defn normalize [dist]
  "Normalizes the dist for computing the probability of a hypo from the dist."
  (let [denom (apply + (vals dist))]
    (into (sorted-map) (reduce-kv (fn [m k v] (assoc m k (/ v denom))) {} dist))))

(defn uniform-distribution [hypos]
  "Creates a distribution with given collection of hypos and each having the same probability."
  (let [ks (cond
             (number? (first hypos)) hypos
             (string? (first hypos)) (map keyword hypos))]
    (normalize (zipmap ks
                       (repeat (count hypos) 1)))))

(defn power-distribution [hypos alpha]
  "Creates a power distribution over the supplied hypotheses."
  (let [ks (cond
             (number? (first hypos)) hypos
             (string? (first hypos)) (map keyword hypos))]
    (normalize (zipmap ks
                       (map (fn [h] (java.lang.Math/pow h (- alpha))) hypos)))))

(defn set-prob [dist hypo pr]
  (assoc dist hypo pr))

(defn prob [dist hypo]
  (get dist hypo))

(defn increment [dist hypo]
  (if (nil? (get dist hypo))
    (conj dist {hypo 1})
    (assoc dist hypo (inc (get dist hypo)))))

(defn mult [dist hypo likelihood]
  "Multiplies the probability of hypo in dist by the given likelihood. Returns the distribution with updated entries."
  (if (nil? (get dist hypo))
    dist
    (assoc dist hypo (* (get dist hypo) likelihood))))

(defn update-prob [dist like-fn data]
  "Generates a new distribution based on the likelihood function, like-fn, and given data."
  (let [updated-dist (loop [hypos (keys dist)
                            mod-dist dist]
                       (if (empty? hypos)
                         mod-dist
                         (recur 
                            (rest hypos)
                            (mult mod-dist (first hypos) (like-fn (first hypos) data)))))]
    (normalize updated-dist)))

(defn mean [dist]
  (float (reduce-kv (fn [total h p]
                     (+ total (* h p))) 0 dist)))

(defn percentile [dist pct-val]
  "This function returns the value at the percentage boundary, pct, in distribution dist."
  (let [pct (/ pct-val 100.0)
        hs (keys dist)
        ps (vals dist)]
    ; A not so elegant loop-recur implementation. I would have preferred a reduce-kv version, but there is a clojure bug
    ; with reduced inside of large map reduce-kv calls.
    (loop [curr-total (first ps)
           curr-hs hs
           curr-ps ps]
      (if (>= curr-total pct)
        (first curr-hs)
        (recur (+ curr-total (first curr-ps)) (rest curr-hs) (rest curr-ps))))))

(defn cdf-from-pmf [pmf-dist]
  "Creates a cumulative distribution function from a provided probability mass function (distribution)."
  (loop [vs (keys pmf-dist)
         ps (vals pmf-dist)
         curr-sum 0.0
         cdf {:vals [] :probs []}]
    (if (empty? vs)
      cdf
      (recur (rest vs) 
             (rest ps)
             (+ curr-sum (first ps))
             (assoc cdf :vals (conj (:vals cdf) (first vs)) :probs (conj (:probs cdf) (+ curr-sum (first ps))))))))

(defn value-of [cdf prob]
  {:pre [(and (>= prob 0) (<= prob 1))]}
  "Determines the value from the cdf that corresponds with desired probability, prob."
  ; Placeholder...
  (println cdf prob))
