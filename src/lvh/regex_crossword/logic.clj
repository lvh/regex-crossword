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

    l/fail)
  (l/== character lvar)
  )

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
