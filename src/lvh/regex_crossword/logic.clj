(ns lvh.regex-crossword.logic
  (:require
   [clojure.core.logic :as l]
   [lvh.regex-crossword.partition :refer [summands partition-by-weights]]
   [com.gfredericks.test.chuck.regexes :as cre]))

(defmulti re->goal :type)

(defmethod re->goal :character
  [{:keys [character]} [lvar :as lvars]]
  (if (-> lvars count (= 1))
    (l/== character lvar)
    l/fail))

(defmethod re->goal :alternation
  [{:keys [elements]} lvars]
  (l/or* (map #(re->goal % lvars) elements)))

(defmethod re->goal :concatenation
  [{:keys [elements]} lvars]
  (let [n-vars (count lvars)
        n-elems (count elements)
        bounds [0 n-vars]]
    (l/or*
     (for [weights (summands n-vars (repeat n-elems bounds))
           :let [lvar-groups (partition-by-weights weights lvars)]]
       (l/and* (map re->goal elements lvar-groups))))))

(defmethod re->goal :repetition
  [{[elem] :elements [lower upper] :bounds} lvars]
  (let [n-vars (count lvars)
        lower (-> lower (or 0))
        upper (-> upper (or n-vars) (max n-vars))]

    ;; Even though e.g. the empty string matches "A*", we get the lvars from the
    ;; structure of the puzzle, so we know all lvars have to be matched.

    ;; Consequence 1: 0 reps only works if there are 0 vars to match.
    (if (zero? n-vars)
      (if (zero? lower) l/succeed l/fail)
      (l/or*
       (for [reps (range (max lower 1) (inc upper))
             ;; Consequence 2: can't have any leftovers: must match all parts
             :when (zero? (rem n-vars reps))
             :let [group-size (quot n-vars reps)
                   groups (partition group-size lvars)]]
         (l/and* (map (partial re->goal elem) groups)))))))

#_(l/run 1 [q]
    (l/and* (regex-goals #"A" [q])))

#_(l/run 1 [q]
    (l/and* (regex-goals [#"A" #"B"] q)))

#_(l/run 1 [q r]
    (l/and* (regex-goals #"(A|B)+" [q r])))

#_(l/run 1 [q]
    (l/== q 'fred))

#_(l/run 1 [q]
    (l/== q 'fred)
    (l/== q 'ethel))

(def dom
  (->> (range (int \A) (inc (int \Z))) (map char)))

(defn solve
  [{:keys [x-patterns y-patterns n] :as opts}]
  (let [n-vars (* (count x-patterns) (count y-patterns))
        vars (repeatedly n-vars l/lvar)
        rows (partition (count x-patterns) vars)
        cols (apply mapv vector rows)
        pattern-goals (mapcat
                       re->goal
                       (concat x-patterns y-patterns)
                       (concat cols rows))]
    (->> (l/run n [q]
           (l/== q vars)
           (l/everyg (fn [v] (l/membero v dom)) vars)
           (l/and* pattern-goals)))))

#_(solve {:x-patterns [#"A"] :y-patterns [#"A"]})
#_(solve {:x-patterns [#"X"] :y-patterns [#"X"]})
#_(solve {:x-patterns [#"A"] :y-patterns [#"A"]} 10)
#_(solve {:x-patterns [#"A"] :y-patterns [#"A|B"]})
#_(solve {:x-patterns [#"A" #"B"] :y-patterns [#"A|B"]})


#_(partition-by-weights [1 2 1] '(p q r s))




(def dom
  (->> (range (int \A) (inc (int \Z))) (map char)))
