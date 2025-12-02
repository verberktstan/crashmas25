(ns crashmas25.day2
  (:require [clojure.edn :as edn]
            [clojure.test :as test]
            [clojure.string :as str]
            [crashmas25.day1 :refer [read-input]]))

(defn- parse-range-input [s]
  (->> (str/split s #"-") (map edn/read-string)))

(defn- silly-id* [{:keys [digit-cnt n-digits digit-str]}]
  (when (= n-digits digit-cnt)
    (reduce = (partition (-> n-digits (/ 2) int) digit-str))))

(defn- silly-id?[n]
  (-> n pos-int? assert)
  (let [digit-str (str n)
        digit-cnt (count digit-str)
        props {:digit-str digit-str :digit-cnt digit-cnt}]
    (when (even? digit-cnt)
      (or (reduce = digit-str)
          (silly-id* (assoc props :n-digits 4))
          (silly-id* (assoc props :n-digits 6))
          (silly-id* (assoc props :n-digits 8))
          (silly-id* (assoc props :n-digits 10))))))

(defn- silly-patterns[coll]
  (set (filter (memoize silly-id?) coll)))

(test/deftest silly-patterns-test
  (test/is (= #{11 22} (silly-patterns (range 11 23))))
  (test/is (= #{99} (silly-patterns (range 95 116))))
  (test/is (= #{1010} (silly-patterns (range 998 1013))))
  (test/is (= #{1188511885} (silly-patterns (range 1188511880 1188511891))))
  (test/is (= #{222222} (silly-patterns (range 222220 222225))))
  (test/is (= #{} (silly-patterns (range 1698522 1698529))))
  (test/is (= #{446446} (silly-patterns (range 446443 446450))))
  (test/is (= #{38593859} (silly-patterns (range 38593856 38593863)))))

(test/deftest day2-test
  (letfn [(day2part1 [filename]
            (->> filename
                 read-input
                 (mapcat #(str/split % #","))
                 (map parse-range-input)
                 (map (juxt first (comp inc second)))
                 (map (partial apply range))
                 (mapcat silly-patterns)
                 (reduce + 0)
                 time))]
    (test/is (-> "day2/test.txt" day2part1 (= 1227775554)))
    (test/is (-> "day2/input.txt" day2part1 (= 30608905813)))))
