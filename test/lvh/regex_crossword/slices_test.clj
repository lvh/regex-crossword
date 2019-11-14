(ns lvh.regex-crossword.slices-test
  (:require [lvh.regex-crossword.slices :as slices]
            [clojure.test :as t]))

(t/deftest hex-row-counts-tests
  ;; built with https://regexcrossword.com/puzzlebuilder
  (t/are [rows cols counts] (= counts
                               (slices/hex-row-counts
                                {:patterns-x (repeat cols [])
                                 :patterns-y (repeat rows [])
                                 :patterns-z (repeat cols [])}))
    1 1 [1]
    1 2 [2]
    3 2 [1 2 1]
    3 3 [2 3 2]
    5 3 [1 2 3 2 1]
    3 4 [3 4 3]))

(t/deftest hex-slices-tests
  ;; Example board (y = 3, x = z = 5)
  ;;
  ;;    A B C D E        ⤢ = "forward"
  ;;   F G H I J K
  ;;    L M N O P        ⤡ = "backward"
  ;;
  ;; xs: AF, BGL, CHM, DIN, EJO, KP ("forward")
  ;; zs: LF, MGA, NHB, OIC, PJD, KE ("backward")
  (let [rows (map seq ["ABCDE" "FGHIJK" "LMNOP"])]
    (t/is (=
           (map seq ["AF" "BGL" "CHM" "DIN" "EJO" "KP"])
           (slices/forward-slices rows)))
    (t/is (=
           (map seq ["LF" "MGA" "NHB" "OIC" "PJD" "KE"])
           (slices/backward-slices rows))))

  ;; Example board 2 (y = 5, x = z = 5)
  ;;
  ;;    A B C
  ;;   D E F G
  ;;  H I J K L
  ;;   M N O P
  ;;    Q R S
  ;; xs: ADH, BEIM, CFJNQ, GKOR, LPS
  ;; zs: QMH, RNID, SOJEA, PKFB, LGC
  (let [rows (map seq ["ABC" "DEFG" "HIJKL" "MNOP" "QRS"])]
    (t/is (=
           (map seq ["ADH" "BEIM" "CFJNQ" "GKOR" "LPS"])
           (slices/forward-slices rows)))
    (t/is (=
           (map seq ["QMH" "RNID" "SOJEA" "PKFB" "LGC"])
           (slices/backward-slices rows)))))
