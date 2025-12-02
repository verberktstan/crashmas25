(ns crashmas25.day1
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :as test]))

(defn- parse-rotation
  "Returns the rotation data parsed from a string like 'L23' to a map with data
  like {:rotation/direction :left, :rotation/steps 23}"
  [s]
  (when-let [direction (and s (some-> s first {\L :left \R :right}))]
    (when-let [steps (some->> s rest (apply str) edn/read-string)]
      {:rotation/steps     steps
       :rotation/operator (get {:left -} direction +)})))

(test/deftest parse-rotation-test
  (test/is (= {:rotation/steps 42 :rotation/operator -}
              (parse-rotation "L42"))))

(defn read-input
  "Reads all the lines from `filename` and returns the lines parsed by `parser`."
  ([filename] (read-input filename nil))
  ([filename parser]
   (with-open [reader (io/reader (str "resources/" filename))]
     (cond->> (-> reader line-seq doall)
       parser (map parser)))))

(defn- wrapped
  "Returns a function that does the same as op (- or +) and wraps it between zero
  and value."
  [op value]
  (-> op #{- +} assert)
  (-> value pos-int? assert)
  (memoize
   (fn wrapped* [a b]
     (let [result (op a b)]
       (cond
         (neg-int? result) (+ result value)
         (>= result value) (- result value)
         :else             result)))))

(test/deftest wrapped-test
  (let [minus (wrapped - 60)
        plus (wrapped + 61)]
    (test/are [a b output] (= output (minus a b))
      30 29 1
      30 30 0
      30 31 59)
    (test/are [a b output] (= output (plus a b))
      30 30 60
      30 31 0
      30 32 1)))

(defn- do-rotation
  "Given a dial and a rotation direction/step, return the dial after rotating."
  [dial {:rotation/keys [steps operator]}]
  (let [steps    (mod steps 100)
        dial     (update dial :dial/at (wrapped operator 100) steps)]
    (cond-> dial
      (some-> dial :dial/at zero?)
      (update :dial/land-on-zero-count inc))))

(test/deftest do-rotation-test
  (let [dial {:dial/at 50 :dial/land-on-zero-count 0}]
    (test/are [input output] (= output (do-rotation dial input))
      {:rotation/operator - :rotation/steps 1}
      (assoc dial :dial/at 49)

      {:rotation/operator + :rotation/steps 1}
      (assoc dial :dial/at 51)

      {:rotation/operator - :rotation/steps 52}
      (assoc dial :dial/at 98)

      {:rotation/operator + :rotation/steps 53}
      (assoc dial :dial/at 3)

      {:rotation/operator - :rotation/steps 50}
      {:dial/at 0 :dial/land-on-zero-count 1})))

(defn- count-zero-crossings
  [dial {:rotation/keys [steps operator]}]
  (let [mod-op   (comp #(mod % 100) operator)
        coll     (reductions mod-op
                             (:dial/at dial)
                             (repeat steps 1))
        zc-count (->> coll rest (filter zero?) count)]
    (-> dial
        (update :dial/at mod-op steps)
        (update :dial/zc-count + zc-count))))

(defn- follow-rotations
  [rotations f]
  (reduce f {:dial/at 50 :dial/land-on-zero-count 0 :dial/zc-count 0} rotations))

(test/deftest day1-test
  (letfn [(day1part1 [filename]
            (-> filename
                (read-input parse-rotation)
                (follow-rotations do-rotation)
                :dial/land-on-zero-count
                time))
          (day1part2 [filename]
            (-> filename
                (read-input parse-rotation)
                (follow-rotations count-zero-crossings)
                :dial/zc-count
                time))]
    (test/is (-> "day1/test.txt"
                 day1part1
                 (= 3)))
    (test/is (-> "day1/input.txt"
                 day1part1
                 (= 1011)))
    (test/is (-> "day1/test.txt"
                 day1part2
                 (= 6)))
    (test/is (-> "day1/input.txt"
                 day1part2
                 (= 5937)))))
