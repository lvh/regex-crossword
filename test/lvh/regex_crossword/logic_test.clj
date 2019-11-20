(ns lvh.regex-crossword.logic-test
  (:require [lvh.regex-crossword.logic :as rcl]
            [com.gfredericks.test.chuck.regexes :as cre]
            [clojure.test :as t]
            [clojure.core.logic :as l])
  (:refer-clojure :exclude [+ *]))

(defn character
  [c]
  {:type :character :character (char c)})

(def ^:private convert-elems
  (partial map (fn [e] (if (char? e) (character e) e))))

(defn |
  [& elems]
  {:type :alternation :elements (convert-elems elems)})

(defn ||
  [& elems]
  {:type :concatenation :elements (convert-elems elems)})

(defn rep
  [lower upper & elems]
  {:type :repetition :elements (convert-elems elems) :bounds [lower upper]})

(def * (partial rep 0 nil))
(def + (partial rep 1 nil))
(def ? (partial rep 0 1))

(def a (character \A))
(def aa (|| a a))

(t/deftest re->goal-character-tests
  (t/is (= '(\A)
           (l/run* [q]
             (rcl/re->goal a [q] {}))))
  (t/is (= '(\A)
           (l/run* [q]
             ;; Note: parser introduces unnecessary alternations/concatenations
             (rcl/re->goal (cre/parse "A") [q] {})))))

(t/deftest re->goal-alternation-test
  (t/is (= '(\A \B)
           (l/run* [q]
             ;; "A|B" with the internal concatenations removed
             (rcl/re->goal (| \A \B) [q] {}))))

  (t/is (= '(\A \B \C)
           (l/run* [q]
             ;; "A|B|C" with the internal concatenations removed
             (rcl/re->goal (| \A \B \C) [q] {}))))

  (t/is (= '(\A \B)
           (l/run* [q]
             ;; Note: parser introduces unnecessary alternations/concatenations
             (rcl/re->goal (cre/parse "A|B") [q] {})))))

(t/deftest re->goal-concatenation-tests
  (t/is (= '((\A \A))
           (l/run* [p q]
             (rcl/re->goal (|| \A \A) [p q] {}))))
  (t/is (= '((\A \A))
           (l/run* [p q]
             (rcl/re->goal (cre/parse "AA") [p q] {})))))

(t/deftest re->goal-repetition-tests
  (t/testing "all lvars must get matched in a repetition"
    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal (* \A) [p] {}))))

    (t/is (= '((\A \A))
             (l/run* [p q]
               (rcl/re->goal (* \A) [p q] {}))))

   (t/is (= '((\A \A \A))
             (l/run* [p q r]
               (rcl/re->goal (* \A) [p q r] {}))))

    (t/is (= '((\A \A \A \A))
             (l/run* [p q r s]
               (rcl/re->goal (* \A) [p q r s] {}))))

    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal (+ \A) [p] {}))))
    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal (? \A) [p] {}))))

    (t/testing "edge cases where repetition has to fit 1 char into 1 lvar"
      (t/is (= '(\A)
               (l/run* [p]
                 (rcl/re->goal (cre/parse "A{1}") [p] {}))))
      (t/is (= '(\A)
               (l/run* [p]
                 (rcl/re->goal (cre/parse "A{1,1}") [p] {}))))
      (t/is (= '((\B \A))
               (l/run* [p q]
                 (rcl/re->goal (cre/parse "BA{1,1}") [p q] {})))))))

(t/deftest re->goal-class-tests
  (t/testing "class-base"
    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal {:type :class-base :chars #{\A}} [p] {})))))

  (t/testing "class-union"
    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal
                {:type :class-union
                 :elements [{:type :class-base :chars #{\A}}]}
                [p] {}))))

    (t/is (= '(\A \B)
             (l/run* [p]
               (rcl/re->goal
                {:type :class-union
                 :elements [{:type :class-base :chars #{\A}}
                            {:type :class-base :chars #{\B}}]}
                [p] {})))))

  (t/testing "class-intersection"
    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal
                {:type :class-intersection
                 :elements [{:type :class-base :chars #{\A}}]}
                [p] {}))))

    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal
                {:type :class-intersection
                 :elements [{:type :class-base :chars #{\A}}
                            {:type :class-base :chars #{\A}}]}
                [p] {}))))

    (t/is (= '()
             (l/run* [p]
               (rcl/re->goal
                {:type :class-intersection
                 :elements [{:type :class-base :chars #{\A}}
                            {:type :class-base :chars #{\B}}]}
                [p] {})))))

  (t/testing "simple examples from parser, no ranges or negation"
    (t/is (= '(\A)
             (l/run* [q]
               (-> "[A]" cre/parse (rcl/re->goal [q] {})))))
    (t/is (= '(\A \B)
             (l/run* [q]
               (-> "[AB]" cre/parse (rcl/re->goal [q] {}))))))

  (t/testing "ranges"
    (t/is (= '(\A \B \C \D \E \F)
             (l/run* [q]
               (-> "[A-F]" cre/parse (rcl/re->goal [q] {})))))
    (t/is (= #{\A \B \C \D \E \F \H \I \J}
             (set
              (l/run* [q]
                (-> "[A-FH-J]" cre/parse (rcl/re->goal [q] {})))))))

  (t/testing "simple classes"
    (t/is (= '(\0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
             (l/run* [q]
               (-> "\\d" cre/parse (rcl/re->goal [q] {})))))
    (t/is (= '(_0)
             (l/run* [q]
               (-> ".*" cre/parse (rcl/re->goal [q] {})))))
    (t/is (= '(_0)
             (l/run* [q]
               (-> "." cre/parse (rcl/re->goal ['q] {})))))
    (t/is (= '(\space)
             (l/run* [q]
               (-> "\\s" cre/parse (rcl/re->goal [q] {}))))))

  (t/testing "simple co-classes (\\D, \\S etc)"
    (t/are [c1 c2] (= '()
                      (l/run* [q]
                        (-> c1 cre/parse (rcl/re->goal [q] {}))
                        (-> c2 cre/parse (rcl/re->goal [q] {}))))
      "\\w" "\\W"
      "\\d" "\\D"
      "\\s" "\\S"))

  (t/testing "negation"
    (t/is (= '(\A)
             (l/run* [q]
               (-> "[ABC]" cre/parse (rcl/re->goal [q] {}))
               (-> "[^B-C]" cre/parse (rcl/re->goal [q] {})))))
    (t/is (= '(\G)
             (l/run* [q]
               (-> "[A-G]" cre/parse (rcl/re->goal [q] {}))
               (-> "[^A-F]" cre/parse (rcl/re->goal [q] {})))))

    (t/is (= '(\D)
             (l/run* [q]
               (-> "[A-G]" cre/parse (rcl/re->goal [q] {}))
               (-> "[^A-C]" cre/parse (rcl/re->goal [q] {}))
               (-> "[^EFG]" cre/parse (rcl/re->goal [q] {})))))

    (t/testing "goals in opposite order"
      ;; This is an interesting test because l/nafc, once used in the
      ;; implementation of :class-negation, only works if the lvars are ground.
      ;; In the first test they're only ground because we ground them
      ;; beforehand. This test checks that the implementation does not require
      ;; that (somehow -- see impl for details).
      (t/is (= '(\G)
               (l/run* [q]
                 (-> "[^A-F]" cre/parse (rcl/re->goal [q] {}))
                 (-> "[A-G]" cre/parse (rcl/re->goal [q] {}))))))))

(t/deftest enumerate-class-tests
  (let [enum (fn [pattern]
               (-> pattern cre/parse :elements first :elements first rcl/enumerate-class))]
    (t/is (= #{\A} (enum "[A]")))
    (t/is (= #{\A \B \C} (enum "[ABC]")))
    (t/is (= #{\A \B \C} (enum "[A-C]")))
    (t/is (= #{\A \B \C \G \H \I} (enum "[A-CG-I]")))))

(t/deftest solve-tests
  (t/is (= [[[\A]]]
           (rcl/solve {:patterns-x [["A"]] :patterns-y [["A"]]})))

  (t/is (= [[[\X]]]
           (rcl/solve {:patterns-x [["X"]] :patterns-y [["X"]]})))

  (t/is (= [[[\A]]]
           (rcl/solve {:patterns-x [["A"]] :patterns-y [["A"]]} 10)))

  (t/is (= [[[\A]]]
           (rcl/solve {:patterns-x [["A"]] :patterns-y [["A|B"]]})))

  (t/is (= [[[\A \B]]]
           (rcl/solve {:patterns-x [["A"] ["B"]] :patterns-y [["A*B*"]]})))

  (t/testing "groups and backrefs"
    (t/is (=
           '(((\A) (\A)))
           (rcl/solve {:patterns-x [["(A)\\1"]] :patterns-y [["A"] ["A"]]})))
    (t/is (=
           '(((\A) (\A) (\A)))
           (rcl/solve {:patterns-x [["(A)\\1\\1"]] :patterns-y [["A"] ["A"] ["A"]]})))))
