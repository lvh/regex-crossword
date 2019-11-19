(ns lvh.regex-crossword.logic
  (:require
   [clojure.core.logic :as l]
   [lvh.regex-crossword.partition :refer [summands partition-by-weights]]
   [com.gfredericks.test.chuck.regexes :as cre]
   [clojure.set :as set]))

(defmulti re->goal (fn [pattern lvars ctx] (:type pattern)))

(defmethod re->goal :character
  [{:keys [character]} [lvar :as lvars] ctx]
  (if (-> lvars count (= 1))
    (l/== character lvar)
    l/fail))

(defmethod re->goal :alternation
  [{:keys [elements]} lvars ctx]
  (l/or* (map #(re->goal % lvars ctx) elements)))

(defmethod re->goal :concatenation
  [{:keys [elements]} lvars ctx]
  (let [n-vars (count lvars)
        n-elems (count elements)
        bounds [0 n-vars]]
    (l/or*
     (for [weights (summands n-vars (repeat n-elems bounds))
           :let [lvar-groups (partition-by-weights weights lvars)]]
       (l/and* (map re->goal elements lvar-groups (repeat ctx)))))))

(defmethod re->goal :repetition
  [{[elem] :elements [lower upper] :bounds} lvars ctx]
  (let [n-vars (count lvars)
        lower (-> lower (or 0))
        upper (-> upper (or n-vars) (max n-vars))]
    ;; Even though e.g. the empty string matches "A*", we get the lvars from the
    ;; structure of the puzzle, so we know all lvars have to be matched.
    ;; Consequence 1: 0 reps only works if there are 0 vars to match.
    (if (zero? n-vars)
      (if (zero? lower) l/succeed l/fail)
      (l/or*
       (for [reps (range (max lower 1) (inc upper))
             ;; Consequence 2: can't have any leftovers: must match all parts,
             ;; group size must be a divisor of the number of lvars
             :when (zero? (rem n-vars reps))
             :let [group-size (quot n-vars reps)
                   groups (partition group-size lvars)]]
         (l/and* (map (partial re->goal elem) groups (repeat ctx))))))))

(defmethod re->goal :class
  [{:keys [elements simple-class]} [lvar :as lvars] ctx]
  ;; Ostensibly only ever one element in elements, but writing defensively.
  (cond
    (-> lvars count (not= 1))
    l/fail

    (some? simple-class)
    (l/membero lvar (case simple-class \s [\space]))

    :else
    (l/and* (map #(re->goal % lvars ctx) elements))))

(defmethod re->goal :class-base
  [{:keys [chars]} [lvar] ctx]
  ;; It appears class-base will only ever have one char, but I'm writing this
  ;; defensively since I have no proof I've exhausted all the parser cases.
  (l/membero lvar (vec chars)))

(defmethod re->goal :class-union
  [{:keys [elements]} lvars ctx]
  (l/or* (map #(re->goal % lvars ctx) elements)))

(defmethod re->goal :class-intersection
  [{:keys [elements]} lvars ctx]
  (l/and* (map #(re->goal % lvars ctx) elements)))

(declare enumerate-class)

(defmethod re->goal :range
  [range [lvar] ctx]
  (->> range enumerate-class vec (l/membero lvar)))

(defmethod re->goal :class-negation
  [{:keys [elements]} [lvar] ctx]
  (l/and*
   (for [not-it (enumerate-class {:type :class :elements elements})]
     (l/!= not-it lvar))))

(defn enumerate-class
  [{:keys [type elements chars simple-class] :as class-spec}]
  (case type
    (:class :class-union)
    (apply set/union (map enumerate-class elements))

    :class-intersection
    (apply set/intersection (map enumerate-class elements))

    :class-base
    (set chars)

    :range
    (let [[lower upper] (map (comp int :character) elements)]
      (into #{} (map char (range lower (inc upper)))))))

(defmethod re->goal :group
  [{:keys [elements]} lvars ctx]
  (swap! (ctx ::groups) conj lvars)
  (re->goal (first elements) lvars ctx))

(defmethod re->goal :unsupported
  [{:keys [unsupported]} lvars ctx]
  (when (not= unsupported #{:backreferences})
    (throw (ex-info "Only backrefs supported" {:unsupported unsupported})))
  ;; HACK: cre/analyze implements backrefs as:
  ;; :BackReference (constantly (unsupported :backreferences)) making no attempt
  ;; to extract the information from the parse. we hack around this by
  ;; pretending there's only one backref.
  (->> ctx ::groups deref (l/member1o lvars)))

(defn solve
  ([puzzle]
   (solve puzzle nil))
  ([{:keys [patterns-x patterns-y] :as puzzle} {:keys [n] :as opts :or {n 10}}]
   (let [n-vars (* (count patterns-x) (count patterns-y))
         vars (repeatedly n-vars l/lvar)
         rows (partition (count patterns-x) vars)
         cols (apply mapv vector rows)
         ctx {::groups (atom [])}
         pattern-goals (map
                        (fn [patterns lvars]
                          (->> patterns
                               (map (fn [pattern] (-> pattern cre/parse (re->goal lvars ctx))))
                               (l/and*)))
                        (concat patterns-x patterns-y)
                        (concat cols rows))]
     (->> (l/run n [q]
            (l/== q rows)
            (l/and* pattern-goals))
          ;; distinct is only necessary for our hacky backref impl
          (distinct)))))
