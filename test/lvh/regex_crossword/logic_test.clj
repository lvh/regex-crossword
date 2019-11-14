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
             (rcl/re->goal a [q]))))
  (t/is (= '(\A)
           (l/run* [q]
             ;; Note: parser introduces unnecessary alternations/concatenations
             (rcl/re->goal (cre/parse "A") [q])))))

(t/deftest re->goal-alternation-test
  (t/is (= '(\A \B)
           (l/run* [q]
             ;; "A|B" with the internal concatenations removed
             (rcl/re->goal (| \A \B) [q]))))

  (t/is (= '(\A \B \C)
           (l/run* [q]
             ;; "A|B|C" with the internal concatenations removed
             (rcl/re->goal (| \A \B \C) [q]))))

  (t/is (= '(\A \B)
           (l/run* [q]
             ;; Note: parser introduces unnecessary alternations/concatenations
             (rcl/re->goal (cre/parse "A|B") [q])))))

(t/deftest re->goal-concatenation-tests
  (t/is (= '((\A \A))
           (l/run* [p q]
             (rcl/re->goal (|| \A \A) [p q]))))
  (t/is (= '((\A \A))
           (l/run* [p q]
             (rcl/re->goal (cre/parse "AA") [p q])))))

(t/deftest re->goal-repetition-tests
  (t/testing "all lvars must get matched in a repetition"
    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal (* \A) [p]))))

    (t/is (= '((\A \A))
             (l/run* [p q]
               (rcl/re->goal (* \A) [p q]))))

   (t/is (= '((\A \A \A))
             (l/run* [p q r]
               (rcl/re->goal (* \A) [p q r]))))

    (t/is (= '((\A \A \A \A))
             (l/run* [p q r s]
               (rcl/re->goal (* \A) [p q r s]))))

    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal (+ \A) [p]))))
    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal (? \A) [p]))))))

(t/deftest re->goal-class-tests
  (t/testing "class-base"
    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal {:type :class-base :chars #{\A}} [p])))))

  (t/testing "class-union"
    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal
                {:type :class-union
                 :elements [{:type :class-base :chars #{\A}}]}
                [p]))))

    (t/is (= '(\A \B)
             (l/run* [p]
               (rcl/re->goal
                {:type :class-union
                 :elements [{:type :class-base :chars #{\A}}
                            {:type :class-base :chars #{\B}}]}
                [p])))))

  (t/testing "class-intersection"
    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal
                {:type :class-intersection
                 :elements [{:type :class-base :chars #{\A}}]}
                [p]))))

    (t/is (= '(\A)
             (l/run* [p]
               (rcl/re->goal
                {:type :class-intersection
                 :elements [{:type :class-base :chars #{\A}}
                            {:type :class-base :chars #{\A}}]}
                [p]))))

    (t/is (= '()
             (l/run* [p]
               (rcl/re->goal
                {:type :class-intersection
                 :elements [{:type :class-base :chars #{\A}}
                            {:type :class-base :chars #{\B}}]}
                [p])))))

  (t/testing "simple examples from parser, no ranges or negation"
    (t/is (= '(\A)
             (l/run* [q]
               (-> "[A]" cre/parse (rcl/re->goal [q])))))
    (t/is (= '(\A \B)
             (l/run* [q]
               (-> "[AB]" cre/parse (rcl/re->goal [q]))))))

  (t/testing "ranges"
    (t/is (= '(\A \B \C \D \E \F)
             (l/run* [q]
               (-> "[A-F]" cre/parse (rcl/re->goal [q])))))
    (t/is (= #{\A \B \C \D \E \F \H \I \J}
             (set
              (l/run* [q]
                (-> "[A-FH-J]" cre/parse (rcl/re->goal [q])))))))

  (t/testing "simple classes"
    (t/is (= '(\A \B \C \D \E \F)
             (l/run* [q]
               (-> "[A-F]" cre/parse (rcl/re->goal [q])))))
    (t/is (= #{\A \B \C \D \E \F \H \I \J}
             (set
              (l/run* [q]
                (-> "[A-FH-J]" cre/parse (rcl/re->goal [q]))))))))

(t/deftest solve-tests
  (t/is (= [[[\A]]]
           (rcl/solve {:x-patterns [["A"]] :y-patterns [["A"]]})))

  (t/is (= [[[\X]]]
           (rcl/solve {:x-patterns [["X"]] :y-patterns [["X"]]})))

  (t/is (= [[[\A]]]
           (rcl/solve {:x-patterns [["A"]] :y-patterns [["A"]]} 10)))

  (t/is (= [[[\A]]]
           (rcl/solve {:x-patterns [["A"]] :y-patterns [["A|B"]]})))

  (t/is (= [[[\A \B]]]
           (rcl/solve {:x-patterns [["A"] ["B"]] :y-patterns [["A*B*"]]}))))
