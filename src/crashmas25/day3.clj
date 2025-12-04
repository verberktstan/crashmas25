(ns crashmas25.day3
  (:require [crashmas25.day1 :refer [read-input]]
            [clojure.edn :as edn]
            [clojure.test :as test]))

(def indexed-char
  (memoize (fn [index char]
             {:battery/index index :battery/value (-> char str edn/read-string)})))

(defn- joltage-for-combo
  "Return the joltage for a combination of batteries."
  [sorted-batteries s {:battery/keys [index value]}]
  (let [coll      (filter #(> (:battery/index %) index) sorted-batteries) ;; TODO: transduce here with take 1?
        new-value (str value (some-> coll first :battery/value))]
    (cond-> s
      (seq coll) (conj (edn/read-string new-value)))))

(defn- largest-joltage
  "Return the largest-joltage for a bank of batteries"
  [battery-bank]
  (let [sorted-batteries (sort-by :battery/value > battery-bank)
        get-joltage      (partial joltage-for-combo sorted-batteries)
        combos           (reduce get-joltage #{} sorted-batteries)]
    (reduce max combos)))

(defn- day3part1 [filename]
  (->> filename
       read-input
       (map (partial map-indexed indexed-char))
       (map largest-joltage)
       (reduce + 0)
       time))

(test/deftest day3part1-test
  (test/is (-> "day3/test.txt" day3part1 (= 357)))
  (test/is (-> "day3/input.txt" day3part1 (= 17524))))
