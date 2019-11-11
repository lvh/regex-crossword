(ns lvh.regex-crossword.partition
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as f]))

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
  ;; We don't need the butlast because map will only use the shortest coll
  ;; argument.
  ;;
  ;; In the special case of a single lvar, we just unify the lvar with the
  ;; result. For most reduce operations this is what you want.
  [binop result lvars]
  (if (-> lvars count (= 1))
    (l/== (first lvars) result)
    (let [results (-> lvars count (- 2) (repeatedly l/lvar) (conj result) reverse)
          lefts (cons (first lvars) results)
          rights (rest lvars)]
      (l/and* (map binop lefts rights results)))))

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

(defn partition-by-weights
  "Partition given coll into colls of length given by `weights`."
  [weights coll]
  (loop [[weight & rest-weights] weights
         chunks []
         remaining coll]
    (if (some? weight)
      (let [[chunk remaining] (split-at weight remaining)]
        (recur rest-weights remaining (conj chunks chunk)))
      chunks)))
