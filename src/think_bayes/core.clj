(ns think-bayes.core)

(defn keys-from-hypos [hypos]
  (cond
    (number? (first hypos)) hypos
    (string? (first hypos)) (map keyword hypos)))

(defn normalize [dist]
  "Normalizes the dist for computing the probability of a hypothesis from the distribution."
  (let [denom (apply + (vals dist))]
    (into (sorted-map) (reduce-kv (fn [m k v] (assoc m k (/ v denom))) {} dist))))

(defn uniform-distribution [hypos]
  "Creates a distribution with given collection of hypotheses (hypos) and each having the same probability."
  (let [ks (keys-from-hypos hypos)]
    (normalize (zipmap ks
                       (repeat (count hypos) 1)))))

(defn power-distribution [hypos alpha]
  "Creates a power distribution over the supplied hypotheses (hypos)."
  (let [ks (keys-from-hypos hypos)]
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

;(defn mult [dist hypo likelihood]
;  "Multiplies the probability of hypothesis (hypo) in distribution (dist) by the given likelihood. 
;Returns the distribution with updated entries."
;  (if (nil? (get dist hypo))
;    dist
;    (assoc dist hypo (* (get dist hypo) likelihood))))

(defn update-prob [dist like-fn data]
  "Generates a new distribution based on the current distribution (dist), likelihood function (like-fn) and given data."
  (let [updated-dist (reduce-kv 
                       (fn [m key val]
                         (assoc m key (* (like-fn key data) val))) {} dist)]
    (normalize updated-dist)))

(defn mean [dist]
  (float (reduce-kv (fn [total h p]
                     (+ total (* h p))) 0 dist)))

(defn maximum-likelihood [dist]
  (key (apply max-key val dist)))

(defn percentile [dist pct-val]
  "This function returns the value at the percentage boundary, pct, in distribution dist."
  (let [pct (/ pct-val 100.0)
        hs (keys dist)
        ps (vals dist)]
    ; A not so elegant loop-recur implementation. I would have preferred a reduce-kv version, but there is a clojure bug
    ; with reduce inside of large map reduce-kv calls.
    (loop [curr-total (first ps)
           curr-hs hs
           curr-ps ps]
      (if (>= curr-total pct)
        (first curr-hs)
        (recur (+ curr-total (first curr-ps)) (rest curr-hs) (rest curr-ps))))))

(defn median [dist]
  (percentile dist 50.0))

(defn credible-interval [dist lb ub]
  (let [low (percentile dist lb)
        high (percentile dist ub)]
    {:upper high, :lower low}))

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

(defn prob-of [cdf value]
  {:pre [(number? value)]}
  "Determines the probability of drawing x <= value from the cumulative distribution."
  (let [idx (.indexOf (:vals cdf) value)]
    (nth (:probs cdf) idx)))