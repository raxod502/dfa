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

(defn strictly-increasing-subseq
  "Returns a lazy subsequence of coll such that (map f <subsequence>)
  is strictly increasing, using the earliest possible values from
  coll. If lower-bound is provided, only elements whose f-values are
  strictly greater than lower-bound are included in the subsequence."
  ([f coll]
   (lazy-seq
     (cons (first coll)
           (strictly-increasing-subseq
             f (rest coll)
             (f (first coll))))))
  ([f coll lower-bound]
   (lazy-seq
     (when (seq coll)
       (let [value (f (first coll))]
         (if (> value lower-bound)
           (cons (first coll)
                 (strictly-increasing-subseq
                   f (rest coll) value))
           (strictly-increasing-subseq
             f (rest coll) lower-bound)))))))
