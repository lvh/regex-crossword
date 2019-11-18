(ns lvh.regex-crossword.cli
  (:require [lvh.regex-crossword.data :as data]
            [lvh.regex-crossword.logic :refer [solve]]
            [clojure.string :as str]
            [com.rpl.specter :as sr]
            [cli-matic.platform])
  (:gen-class))

(defn display
  [rows]
  ;; All rows are the same length for rectangular grids but not for hex grids,
  ;; and this function will work on either.
  (let [rows (map (partial str/join " ") rows)
        max-len (apply max (map count rows))
        center (fn [row]
                 (let [pad (-> (- max-len (count row)) (quot 2) (repeat " ") str/join)]
                   (str pad row pad)))]
    (->> rows (map center) (str/join "\n"))))

(defn -main
  [puzzle-type & args]
  (case puzzle-type
    "builtin"
    (let [set-id? (->> args first re-pattern (partial re-matches))
          puzzle-num (-> args second (Integer/parseInt))]
      (doseq [puzzle (sr/select
                      [sr/ALL (comp set-id? :id) :puzzles (sr/nthpath puzzle-num)]
                      @data/builtin-puzzles)]
        (println "Solving" puzzle)
        (doseq [[solution n] (map vector (solve puzzle) (range))]
          (println "Solution #" n)
          (println (display solution))
          (println "\n"))))
    "player" (throw (RuntimeException. "not implemented"))))

#_(-main "builtin" "tutorial" "0")
#_(-main "builtin" "tutorial" "1")
#_(-main "builtin" "tutorial" "2") ;; negation
#_(-main "builtin" "tutorial" "3")
#_(-main "builtin" "tutorial" "4")
#_(-main "builtin" "tutorial" "5")
#_(-main "builtin" "tutorial" "6") ;; backrefs
#_(-main "builtin" "tutorial" "7")
