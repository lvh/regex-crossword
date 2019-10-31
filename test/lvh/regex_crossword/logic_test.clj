(ns lvh.regex-crossword.logic-test
  (:require [lvh.regex-crossword.logic :as rcl]
            [com.gfredericks.test.chuck.regexes :as cre]
            [clojure.test :as t]
            [clojure.core.logic :as l]))

(t/deftest re->goal-character-test
  (t/is (= '(\A)
           (l/run* [q]
             (rcl/re->goal (cre/parse "A") [q])))))

#_(t/deftest re->goal-concatenation-test
  (t/is (= '((\A \A))
           (l/run* [p q]
             (rcl/re->goal (cre/parse "AA") [p q])))))

(t/deftest re->goal-alternation-test
  (t/is (= '(\A \B)
           (l/run* [q]
             ;; "A|B" with the internal concatenations removed
             (rcl/re->goal {:type :alternation
                            :elements
                            [{:type :character :character \A}
                             {:type :character :character \B}]}
                           [q]))))

  (t/is (= '(\A \B \C)
           (l/run* [q]
             ;; "A|B|C" with the internal concatenations removed
             (rcl/re->goal {:type :alternation
                            :elements
                            [{:type :character :character \A}
                             {:type :character :character \B}
                             {:type :character :character \C}]}
                           [q]))))

  #_(t/is (= '(\A \B)
             (l/run* [q]
               (rcl/re->goal (cre/parse "A|B") [q])))))

(t/deftest summands-test
  (t/is
   (= #{[1 1 1]}
      (set (rcl/summands 3 [[1 10] [1 10] [1 10]]))))
  (t/is
   (= #{[7 5 3] [8 5 2] [7 6 2] [7 7 1] [8 6 1]}
      (set (rcl/summands 15 [[7 8] [5 8] [1 5]])))))
