(ns lvh.regex-crossword.logic
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as f]))

(def dom (apply f/domain (range (int \A) (inc (int \Z)))))

(defn solve
  ([opts]
   (solve opts 1))
  ([{:keys [x-patterns y-patterns] :as opts} n]
   (let [n-vars (* (count x-patterns) (count y-patterns))
         vars (repeatedly n-vars l/lvar)
         xs (partition (count x-patterns) vars)
         ys (apply mapv vector xs)]
     (->> (l/run n [q]
            (l/== q vars)
            (l/everyg (fn [v] (f/in v dom)) vars))
          (map #(map char %))))))

(solve {:x-patterns ["A"] :y-patterns ["A"]})
