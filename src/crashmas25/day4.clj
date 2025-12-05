(ns crashmas25.day4
  (:require [clojure.test :as test]
            [crashmas25.day1 :refer [read-input]]))

(defn- parse-row [s]
  (->> s (map-indexed (fn [i c] {:x i :value ({\. :empty \@ :roll} c)}))))

(defn- lookup-diagram [coll]
  (->> coll
       (map-indexed (fn [i s] (map (partial merge {:y i}) (parse-row s))) )
       (mapcat (partial map (juxt (juxt :x :y) :value)))
       (into {})))

(defn- adjacent-rolls [position lookup]
  (let [adjacents [[-1 -1] [-1 0] [-1 1]
                   [0 -1] [0 1]
                   [1 -1] [1 0] [1 1]]
        positions (map (partial mapv + position) adjacents)]
    (select-keys lookup positions)))

(defn- lookup-frequencies [lookup]
  (fn [m k] (assoc m k (-> k (adjacent-rolls lookup) vals frequencies))))

(defn- accessed-by-forklift [filename]
  (let [lookup (->> filename read-input lookup-diagram)]
    (->> lookup
         (filter (comp #{:roll} val))
         keys
         (reduce (lookup-frequencies lookup) nil)
         (filter (comp #(< (or % 3) 4) :roll val))
         time)))

(test/deftest day4part1-test
  (test/is (= 13 (-> "day4/test.txt" accessed-by-forklift count)))
  (test/is (= 1474 (-> "day4/input.txt" accessed-by-forklift count))))

(defn- to-be-removed-by-forklift [filename]
  (loop [lookup (->> filename read-input lookup-diagram)
         to-be-removed #{}
         i 144] ;; Just to avoid infinite recursion
    (let [positions-to-be-removed (->> lookup
                                       (filter (comp #{:roll} val))
                                       keys
                                       (reduce (lookup-frequencies lookup) nil)
                                       (filter (comp #(< (or % 3) 4) :roll val))
                                       keys)]
      (if (or (zero? i) (not (seq positions-to-be-removed)))
        to-be-removed
        (recur
          (apply dissoc lookup positions-to-be-removed)
          (into to-be-removed positions-to-be-removed)
          (dec i))))))

(test/deftest day4part2-test
  (test/is (= 43 (-> "day4/test.txt" to-be-removed-by-forklift count time)))
  (test/is (= 8910 (-> "day4/input.txt" to-be-removed-by-forklift count time))))
