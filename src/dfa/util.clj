(ns dfa.util)

(defn weighted-rand-nth
  "Returns an item chosen randomly from coll, where the weight for
  each item is given by the corresponding number in weights."
  [coll weights]
  (let [choice (rand (apply + weights))]
    (->> weights
      (reductions +)
      (map (fn [item cumulative]
             (when (< choice cumulative)
               item))
           coll)
      (some identity))))
