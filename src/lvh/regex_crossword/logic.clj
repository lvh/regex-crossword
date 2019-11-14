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
             ;; Consequence 2: can't have any leftovers: must match all parts,
             ;; group size must be a divisor of the number of lvars
             :when (zero? (rem n-vars reps))
             :let [group-size (quot n-vars reps)
                   groups (partition group-size lvars)]]
         (l/and* (map (partial re->goal elem) groups)))))))


(defn solve
  ([puzzle]
   (solve puzzle nil))
  ([{:keys [patterns-x patterns-y] :as puzzle} {:keys [n] :as opts :or {n 10}}]
   (let [n-vars (* (count patterns-x) (count patterns-y))
         vars (repeatedly n-vars l/lvar)
         rows (partition (count patterns-x) vars)
         cols (apply mapv vector rows)
         pattern-goals (map
                        (fn [patterns lvars]
                          (->> patterns
                               (map (fn [pattern] (-> pattern cre/parse (re->goal lvars))))
                               (l/and*)))
                        (concat patterns-x patterns-y)
                        (concat cols rows))]
     (->> (l/run n [q]
            (l/== q rows)
            (l/and* pattern-goals))))))
