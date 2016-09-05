(ns dfa.core
  (:require [clojure.string :as str]
            [dfa.util :as util]))

;;; A DFS is represented as a "transitions" map and an "accepting"
;;; map, which have the same keys, together with an "initial state".
;;; The keys of the transitions and accepting maps represent the
;;; states of the DFS. The values of the transitions map are
;;; themselves maps, and each is a map from possible inputs to other
;;; state keys. The values of the accepting map are boolean values
;;; that specify whether the corresponding states are accepting states
;;; (true) or not (false). The initial state is one of the state keys.

;;; When the DFS is viewed as a single unit, it is a map where the
;;; constituent pieces are under the :transitions, :accepting?, and
;;; :initial keys.

;;;; Sample data

(def even-odd-dfa
  "DFA that accepts only bitstrings representing even numbers."
  {:transitions {:q0 {\0 :q0
                      \1 :q1}
                 :q1 {\0 :q0
                      \1 :q1}}
   :accepting? {:q0 true
                :q1 false}
   :initial :q0})

(defn even-odd-predicate
  "Returns true if its argument is a bitstring representing an even
  number, and false otherwise."
  [bitstring]
  (or (empty? bitstring)
      (= (last bitstring) \0)))

(def bit-counting-dfa
  "DFA that accepts only bitstrings with at least two 0's and at most
  one 1."
  {:transitions {:q0 {\0 :q1
                      \1 :q3}
                 :q1 {\0 :q2
                      \1 :q4}
                 :q2 {\0 :q2
                      \1 :q5}
                 :q3 {\0 :q4
                      \1 :q6}
                 :q4 {\0 :q5
                      \1 :q6}
                 :q5 {\0 :q5
                      \1 :q6}
                 :q6 {\0 :q6
                      \1 :q6}}
   :accepting? {:q0 false
                :q1 false
                :q2 true
                :q3 false
                :q4 false
                :q5 true
                :q6 false}
   :initial :q0})

(defn bit-counting-predicate
  "Returns true if its argument is a bitstring with at least two 0's
  and at most one 1, and false otherwise."
  [bitstring]
  (and (>= (count (filter #{\0} bitstring)) 2)
       (<= (count (filter #{\1} bitstring)) 1)))

(def standard-weights
  "Standard weights for mutating a DFA."
  {:change-transition 100
   :change-accepting 10
   :change-initial 1
   :add-state 1
   :remove-state 2})

(defn double-zero-predicate
  "Returns true if its argument is a bitstring ending in two 0's,
  and false otherwise."
  [bitstring]
  (str/ends-with? (apply str bitstring) "00"))

;;;; Generating data

(defn bitstrings
  "Returns a sequence of all the possible bitstrings of the specified
  length (possibly zero), in lexicographic order."
  [length]
  (->> [[]]
    (iterate (fn [seqs]
               (mapcat (fn [seq]
                         [(conj seq \0)
                          (conj seq \1)])
                       seqs)))
    (drop length)
    (first)
    (map (partial apply str))))

(defn all-bitstrings
  "Returns a sequence of all the possible bitstrings of the specified
  length (possibly zero) or shorter, sorted first by length and then
  lexicographically."
  [max-length]
  (mapcat bitstrings
          (range (inc max-length))))

(defn state-name
  "Returns a keyword that can be used as a name for a DFA state. The
  returned keyword is guaranteed to not be in states. In order of
  preference, this function tries :q0, :q1, ... until it finds an
  unused name."
  [states]
  (->> (range)
    (map #(keyword (str "q" %)))
    (remove (set states))
    (first)))

;;;; DFAs

(defn run
  "Returns true if the DFA accepts the provided input sequence, and
  false otherwise."
  [dfa input-seq]
  (get-in dfa
          [:accepting?
           (reduce (fn [state input]
                     (get-in dfa
                             [:transitions state input]))
                   (:initial dfa)
                   input-seq)]))

(defn accuracy
  "Returns a floating-point number between zero and one representing
  the fraction of the time that the DFA returned the same value as the
  predicate when run on the provided inputs. Note that the predicate
  is just a function of one argument that returns true or false."
  ;; Note: taking a random sampling of bitstrings instead of checking
  ;; every single one may drastically improve performance.
  [dfa predicate inputs]
  (->> inputs
    (map (fn [input]
           (= (run dfa input)
              (predicate input))))
    (remove false?)
    (count)
    (#(/ % (count inputs)))
    (double)))

(def actions
  "Possible actions for mutating a DFA. These should be the keys of
  the weight maps passed to `mutate`."
  [:change-transition
   :change-accepting
   :change-initial
   :add-state
   :remove-state])

(defn mutate
  "Mutates the DFA in some way. If weights-or-action is a keyword, it
  should be one of `actions`. Otherwise, it must be a map from the
  keys in `actions` to weights for random selection, and an action is
  selected using `dfa.util/weighted-rand-nth`. Returns the new DFA.
  Note that the new DFA might be identical to the old one, depending
  on what exactly is mutated and how."
  {:arglists '([dfa weights-or-action])}
  ([{:keys [transitions] :as dfa} weights-or-action]
   (if (keyword? weights-or-action)
     (let [action weights-or-action
           states (keys transitions)
           inputs (-> transitions vals first keys)]
       (case action
         :change-transition
         (assoc-in dfa
                   [:transitions
                    (rand-nth states)
                    (rand-nth inputs)]
                   (rand-nth states))

         :change-accepting
         (assoc-in dfa
                   [:accepting?
                    (rand-nth states)]
                   (rand-nth [false true]))

         :change-initial
         (assoc dfa :initial (rand-nth states))

         :add-state
         (let [new-state (state-name states)
               states (cons new-state states)]
           (-> dfa
             (assoc-in [:transitions
                        new-state]
                       (reduce (fn [transitions input]
                                 (assoc transitions
                                        input
                                        (rand-nth states)))
                               {}
                               inputs))
             (assoc-in [:accepting? new-state]
                       (rand-nth [false true]))))

         :remove-state
         (if (> (count states) 1)
           (let [state (rand-nth states)]
             (-> dfa
               (update :transitions dissoc state)
               (update :accepting? dissoc state)
               (cond-> (= (:initial dfa) state)
                 (mutate :change-initial))))
           dfa)))
     (let [weights weights-or-action
           action (util/weighted-rand-nth
                    actions
                    (map weights actions))]
       (mutate dfa action)))))

;;;; Genetic algorithms

(defn evolve
  "Runs a population through one step of evolution. The population
  should be a map from members to their fitness numbers; fitness
  should be a function of one variable that returns a number for a
  member of the population (larger is better); and mutate should be a
  function of one variable that returns a slightly modified version of
  a member of the population. The population will be culled to at most
  max-size members, if necessary."
  [population mutate fitness max-size]
  (let [new-member (mutate (rand-nth (keys population)))]
    (if-not (contains? population new-member)
      (-> population
        ;; If the population is already at its limit, cull.
        (cond-> (>= (count population) max-size)
          ;; Remove the least fit member of the population.
          (dissoc (key (apply min-key val population))))
        (assoc new-member (fitness new-member)))
      population)))

(defn engineer
  "Quick and dirty function for viewing the progress of evolution."
  [predicate max-length weights max-size]
  (let [inputs (all-bitstrings max-length)
        mutate #(->> (mutate % weights)
                  (fn [])
                  (repeatedly)
                  (remove #{%})
                  (first))
        fitness #(accuracy % predicate inputs)
        initial-dfa {:transitions {:q0 {\0 :q0
                                        \1 :q0}}
                     :accepting? {:q0 false}
                     :initial :q0}]
    (->> {initial-dfa (fitness initial-dfa)}
      (iterate (fn [population]
                 (evolve population mutate fitness max-size)))
      (map (fn [population]
             (apply max-key
                    #(- (val %)
                        (/ (count (str (key %)))
                           1000000.0))
                    population)))
      (distinct)
      (map ./pprint)
      (dorun))))
