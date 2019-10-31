(ns lvh.regex-crossword.logic
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as f]
   [com.gfredericks.test.chuck.regexes :as cre]))

(declare summands)

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
     (map
      (fn [elem weights]
        )
      elements
      (summands n-vars (repeat n-elems bounds))))))

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

(defn reduceo
  "Given a binary operator goal, return an n-ary one.

  If the given goal has shape `(⊕ x y z)` meaning `x ⊕ y = z`, `(reduceo ⊕ z
  vars)` computes `z = vars[0] ⊕ vars[1] ⊕ vars[2]...`."
  ;; lower case are input vars (a, b, c, d...)
  ;; upper case are intermediate accumulator lvars ("running totals")
  ;; Ω is the final result
  ;;     a ⊕ b = A
  ;;     A ⊕ c = B
  ;;       ...
  ;;     W ⊕ y = X
  ;;     X ⊕ z = Ω
  ;;     |   |   |
  ;;     |   |   \_ (concat accumulators (list result))
  ;;     |   \_ (rest lvars)
  ;;     \_ (cons (first lvars) (butlast accumulators))
  ;;
  ;; There are two fewer accumulators than there are input lvars. The middle
  ;; equations all use 2 accumulators each: one to carry the previous result and
  ;; one to carry it to the next equation. The first equation uses 2 input vars
  ;; and 1 accumulator, and the last equation uses 1 input var, 1 accumulator
  ;; and the result (which may be an lvar, but it's not _our_ lvar -- and in
  ;; common uses we expect it to be a normal value).
  ;;
  ;; We don't need the butlast because map will only use the shortest coll argument.
  [binop result lvars]
  (let [results (-> lvars count (- 2) (repeatedly l/lvar) (conj result) reverse)
        lefts (cons (first lvars) results)
        rights (rest lvars)]
    (l/and* (map binop lefts rights results))))

(def sumo (partial reduceo f/+))
(def concato (partial reduceo l/appendo))

(defn summands
  "Find ways you can add some numbers up to a given total.

  For each number (summand) provide bounds `[min max]` or `nil` if you don't
  know. Possible assignments are returned in the same order."
  [total bounds]
  (let [lvars (repeatedly (count bounds) l/lvar)
        bounds-goals (map
                      (fn [v [min max]]
                        (f/in v (f/interval (or min 0) (or max total))))
                      lvars bounds)]
    (l/run* [q]
      (l/== q lvars)
      (l/and* bounds-goals)
      (sumo total lvars))))

(defn ^:private partition-by-weights
  [weights coll]
  (loop [[weight & rest-weights] weights
         coll coll
         acc []]
    (if (some? weight)
      (recur rest-weights
             (drop weight coll)
             (conj acc (take weight coll)))
      acc)))


#_(partition-by-weights [1 2 1] '(p q r s))



;; => ([2 3 5] [3 2 5] [2 4 4] [2 5 3] [4 2 4] [3 3 4] [3 4 3] [5 2 3] [4 3 3] [3 5 2] [4 4 2] [5 3 2])

#_(l/run 10 [a b c]
  (f/in a b c (f/interval 10))
  (sumo 10 [a b c]))

#_(l/run 10 [a b c]
  (concato '(p q r s) [a b c]))


#_(partition-by-weights [1 2 3] '(a b c d e f))


(def dom
  (->> (range (int \A) (inc (int \Z))) (map (comp symbol str char))))
