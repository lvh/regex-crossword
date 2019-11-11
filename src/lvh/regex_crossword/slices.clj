(ns lvh.regex-crossword.slices)

(defn hex-row-counts
  "Given a hex puzzle, count the number of elements in each row."
  [{:keys [x-patterns y-patterns z-patterns :as puzzle]}]
  (let [cols (count x-patterns)
        bottom-half (-> y-patterns count (quot 2) (take (range (dec cols) 0 -1)))]
    (concat (reverse bottom-half) [cols] bottom-half)))

(defn ^:private nth*
  "Like [[clojure.core/nth]] but returns nil on IndexOutOfBoundsException."
  [& args]
  (try (apply nth args) (catch IndexOutOfBoundsException e nil)))

(def ^:private northeast
  "Heading southwest on an even-r offset hex grid. See [[forward-slices]]."
  (cycle [[-1 0] [-1 1]]))

(def ^:private southwest
  "Heading southwest on an even-r offset hex grid. See [[forward-slices]]."
  (cycle [[1 -1] [1 0]]))

(defn forward-slices
  "Take forward slices (think forward slash: /) out of a hex grid."
  [rows]
  ;; Idea: start at the middle row, grow outwards. We're taking forward
  ;; slices (think forward slash, /) here, so we're on the NE-to-SW axis. This
  ;; means increasing X axis in the default game orientation.

  ;; Example board:
  ;;    A B C D E
  ;;   F G H I J K  ⤢
  ;;    L M N O P

  ;;    A B C
  ;;   D E F G
  ;;  H I J K L
  ;;   M N O P
  ;;    Q R S

  ;; The math here gets tricky. https://www.redblobgames.com/grids/hexagons/ for
  ;; details.

  ;; Try not to think "x" and "y" even though the game does that. The hex grid
  ;; doesn't have those coordinates so it gets confusing. We're dealing with a
  ;; seq of rows (increasing y coordinate in the game), first coordinate is the
  ;; row, second is index within the row. That's the opposite
  ;; convention (vertical first) from the above source, so our coordinates will
  ;; be flipped.

  ;; This is an even-r horizontal layout, so going NE means [-1 0] on
  ;; odd rows, [-1 +1] on even rows. Going SW means [+1 -1] on odd rows, [+1 0]
  ;; on even ones. Because of symmetry, the median row is always odd.
  (let [middle-idx (-> rows count (quot 2))
        middle-row (nth rows middle-idx)
        get-elem (fn [[row-idx col-idx]]
                   (let [row (nth* rows row-idx)
                         ;; Our grid is hexagon shaped but we're storing it as a
                         ;; rows-cols seq-of-seqs, so the outer rows "start" at
                         ;; zero but are really centered. This computes that
                         ;; centering offset.
                         col-offset (-> row-idx (- middle-idx) Math/abs (quot 2))]
                     (nth* row (- col-idx col-offset))))]
    (for [col (range (count middle-row))
          :let [base-pos [middle-idx col]
                base (get-elem base-pos)
                walk-until-edge (fn [direction] (->> direction
                                                    (reductions (partial map +) base-pos)
                                                    rest ;; don't include base
                                                    (map get-elem)
                                                    (take-while some?)))
                ne-path (walk-until-edge northeast)
                sw-path (walk-until-edge southwest)]]
      ;; We reverse the ne-path because the algorithm walks from the base out,
      ;; but we're returning the results from NE to SW.
      (concat (reverse ne-path) [base] sw-path))))

(def backward-slices
  "Like [[forward-slices]], but NW-SE oriented (think backslash: \\)."
  (comp forward-slices reverse))
