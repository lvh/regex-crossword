(ns lvh.regex-crossword.logic
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as f]
   [com.gfredericks.test.chuck.regexes :as cre]
   [com.gfredericks.test.chuck.generators :as cgen]
   [clojure.test.check.generators :as gen]))

(def dom
  (->> (range (int \A) (inc (int \Z)))
       (map char)))

(defn matching-strings
  [re len]
  (as-> re $
    (cre/gen-string-from-regex $)
    (cgen/cap-size len $)
    (gen/such-that (fn [s] (-> s count (= len))) $ 1000)))

(defn strings-to-sets
  [strings]
  ;;              (transpose)      (unique)
  (->> strings (apply map vector) (map set)))

(defn enumerate
  [coll]
  (map vector coll (range)))

(defn regex-goals
  [pattern target-vars]
  (let [sets (-> pattern
                 (matching-strings (count target-vars))
                 (gen/sample 1000)
                 (strings-to-sets))]
    (map l/membero target-vars (map seq sets))))

#_(l/run 1 [q]
    (l/and* (regex-goals #"A" [q])))

#_(l/run 1 [q]
    (l/and* (regex-goals [#"A" #"B"] q)))

#_(l/run 1 [q r]
    (l/and* (regex-goals #"(A|B)+" [q r])))

(defn solve
  ([opts]
   (solve opts 1))
  ([{:keys [x-patterns y-patterns] :as opts} n]
   (let [n-vars (* (count x-patterns) (count y-patterns))
         vars (repeatedly n-vars l/lvar)
         rows (partition (count x-patterns) vars)
         cols (apply mapv vector rows)
         pattern-goals (mapcat
                        regex-goals
                        (concat x-patterns y-patterns)
                        (concat cols rows))]
     (->> (l/run n [q]
            (l/== q vars)
            (l/everyg (fn [v] (l/membero v dom)) vars)
            (l/and* pattern-goals))))))

#_(solve {:x-patterns [#"A"] :y-patterns [#"A"]})
#_(solve {:x-patterns [#"X"] :y-patterns [#"X"]})
#_(solve {:x-patterns [#"A"] :y-patterns [#"A"]} 10)
#_(solve {:x-patterns [#"A"] :y-patterns [#"A|B"]})
#_(solve {:x-patterns [#"A" #"B"] :y-patterns [#"A|B"]})
