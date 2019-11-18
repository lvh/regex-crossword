(ns lvh.regex-crossword.cli-test
  (:require [lvh.regex-crossword.cli :as cli]
            [clojure.test :as t]
            [clojure.string :as str]))

(t/deftest display-test
  (t/is (= "A" (cli/display ["A"])))

  (t/is (= "A B" (cli/display ["AB"])))

  (t/is (= "A B\nC D" (cli/display ["AB" "CD"])))

  (t/is (= (->> [" A B C D E "
                 "F G H I J K"
                 " L M N O P "]
                (str/join "\n"))
           (cli/display ["ABCDE" "FGHIJK" "LMNOP"]))
        "3x6 hex")
  (t/is (=
         (->>
          ["  A B C  "
           " D E F G "
           "H I J K L"
           " M N O P "
           "  Q R S  "]
          (str/join "\n"))
         (cli/display ["ABC" "DEFG" "HIJKL" "MNOP" "QRS"]))
        "5x5 hex"))
