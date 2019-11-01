(ns lvh.regex-crossword.logic-test
  (:require [lvh.regex-crossword.logic :as rcl]
            [com.gfredericks.test.chuck.regexes :as cre]
            [clojure.test :as t]
            [clojure.core.logic :as l]))

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

(def a (char \A))
(def aa (|| a a))

(t/deftest re->goal-character-test
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
             (rcl/re->goal (cre/parse "A|B") [q])))))

(t/deftest re->goal-concatenation-test
  (t/is (= '((\A \A))
           (l/run* [q]
             (rcl/re->goal (|| \A \A)))))
  (t/is (= '((\A \A))
           (l/run* [p q]
             (rcl/re->goal (cre/parse "AA") [p q]))))
  (t/is (= '((\A \A))
           (l/run* [p q]
             (rcl/re->goal (cre/parse "AA") [p q])))))

(t/deftest summands-test
  (t/is
   (= #{[1 1 1]}
      (set (rcl/summands 3 [[1 10] [1 10] [1 10]]))))
  (t/is
   (= #{[7 5 3] [8 5 2] [7 6 2] [7 7 1] [8 6 1]}
      (set (rcl/summands 15 [[7 8] [5 8] [1 5]])))))
