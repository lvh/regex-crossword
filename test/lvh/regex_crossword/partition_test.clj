(ns lvh.regex-crossword.partition-test
  (:require
   [lvh.regex-crossword.partition :as rcp]
   [clojure.test.check :as tc]
   [clojure.test :as t]
   [clojure.core.logic :as l]
   [clojure.test.check.generators :as gen]
   [com.gfredericks.test.chuck.generators :as cgen]
   [com.gfredericks.test.chuck.clojure-test :refer [checking]]
   [clojure.core.logic.fd :as f]))

(t/deftest sumo-test
  (t/is
   (= #{[10]}
      (set (l/run 10 [a]
             (f/in a (f/interval 10))
             (rcp/sumo 10 [a])))))

  (t/is
   (= #{[0 0 10] [1 0 9] [0 1 9] [0 2 8] [2 0 8] [1 1 8] [0 3 7] [0 4 6] [1 2 7] [0 5 5]}
      (set (l/run 10 [a b c]
             (f/in a b c (f/interval 10))
             (rcp/sumo 10 [a b c]))))))

(t/deftest ^:generative sumo-generative-test
  (checking
   "sumo produces numbers that add up to the right total" 100
   [total gen/nat
    :let [sums (l/run* [a b c]
                 (f/in a b c (f/interval total))
                 (rcp/sumo total [a b c]))]]
   (t/is (every? (fn [sum] (every? #(<= 0 % total) sum)) sums))
   (t/is (every? #(= total (apply + %)) sums))))

(t/deftest summands-test
  (t/is
   (= #{[1 1 1]}
      (set (rcp/summands 3 [[1 10] [1 10] [1 10]]))))
  (t/is
   (= #{[7 5 3] [8 5 2] [7 6 2] [7 7 1] [8 6 1]}
      (set (rcp/summands 15 [[7 8] [5 8] [1 5]])))))

(t/deftest ^:generative summands-generative-test
  (checking
   "summands creates numbers that add up to the right total" 100
   [total gen/nat
    bounds (gen/vector
            (gen/fmap
             (fn [x] [0 x])
             (gen/choose 0 total))
            1 (max 1 total))
    :let [summands (rcp/summands total bounds)]]
   (t/is (every? #(= total (apply + %)) summands))
   (t/is (every? #(= (count bounds) (count %)) summands))))

(rcp/summands 1 [[0 0]])

(t/deftest partition-by-weights-test
  (t/is (= '((a) (b c) (d e f))
           (rcp/partition-by-weights [1 2 3] '(a b c d e f)))))
