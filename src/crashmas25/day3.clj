(ns crashmas25.day3
  (:require [crashmas25.day1 :refer [read-input]]
            [clojure.edn :as edn]
            [clojure.test :as test]))

(def indexed-char
  (memoize (fn [index char]
             {:battery/index index :battery/value (-> char str edn/read-string)})))

(defn- filter-batteries
  ([battery batteries]
   (filter-batteries 1 battery batteries))
  ([n {:battery/keys [index]} batteries]
   (into [] (comp (filter #(> (:battery/index %) index)) (take n)) batteries)))

(defn- joltage-for-combo
  "Return the joltage for a combination of batteries."
  ([sorted-batteries s battery]
   (joltage-for-combo 1 sorted-batteries s battery))
  ([n sorted-batteries s {:battery/keys [value] :as battery}]
   (let [coll      (filter-batteries n battery sorted-batteries)
        ;; TODO: For part 2 the new-value should consist of 12 digits instead of 2. w00t
         new-value (->> coll (keep :battery/value) (apply str value))]
     (cond-> s
       (seq coll) (conj (edn/read-string new-value))))))

(defn- largest-joltage
  "Return the largest-joltage for a bank of batteries"
  [battery-bank]
  (let [sorted-batteries (sort-by :battery/value > battery-bank)
        get-joltage      (partial joltage-for-combo sorted-batteries)
        combos           (reduce get-joltage #{} sorted-batteries)]
    (some->> combos seq (reduce max))))

(defn- day3part1 [filename]
  (->> filename
       read-input
       (map (partial map-indexed indexed-char))
       (keep largest-joltage)
       (reduce + 0)
       time))

(test/deftest day3part1-test
  (test/is (-> "day3/test.txt" day3part1 (= 357)))
  (test/is (-> "day3/input.txt" day3part1 (= 17524))))
