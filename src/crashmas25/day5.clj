(ns crashmas25.day5
  (:require [clojure.test :as test]
            [crashmas25.day1 :refer [read-input]]
            [clojure.edn :as edn]))

(def fresh-range-matcher #"(\d+)\-(\d+)")
(def id-matcher #"\d+")

(defn- parse-numbers [s]
  (or (some->> (re-matches fresh-range-matcher s)
               rest
               (map edn/read-string)
               (zipmap [:from :to]))
      (some->> s (re-matches id-matcher) edn/read-string (assoc {} :id))))

(test/deftest parse-numbers-test
  (test/testing "Parse-numbers returns line as parsed data"
    (test/is (= {:from 2 :to 34} (parse-numbers "2-34")))
    (test/is (= {:id 56} (parse-numbers "56")))
    (test/is (= [{:from 3, :to 5}
                 {:from 10, :to 14}
                 {:from 16, :to 20}
                 {:from 12, :to 18}
                 nil
                 {:id 1}
                 {:id 5}
                 {:id 8}
                 {:id 11}
                 {:id 17}
                 {:id 32}]
                (read-input "day5/test.txt" parse-numbers)))))

(defn- fresh? [id ranges]
  (->> ranges
       (into [] (comp (filter #(<= (:from %) id (:to %))) (take 1)))
       seq
       boolean))

(defn- read-data [filename]
  (let [coll (read-input filename parse-numbers)
        ranges (filter :from coll)]
    {:coll coll :ranges ranges}))

(defn- fresh-ids [{:keys [coll ranges]}]
  (time (->> coll
             (keep :id)
             (filter #(fresh? % ranges)))))

(test/deftest day5part1-test
  (test/is (= 3 (-> "day5/test.txt" read-data fresh-ids count)))
  (test/is (= 756 (-> "day5/input.txt" read-data fresh-ids count))))

(defn- any-overlap? [{:keys [from to]} ranges]
  (some->> ranges
           (filter (fn [{from2 :from to2 :to}]
                     (or (<= from2 from to2)
                         (<= from2 to to2)
                         (<= from from2 to2 to)
                         (<= from2 from to to2))))
           seq))

(test/deftest any-overlap?-test
  (test/is (= [{:from 2 :to 3}]
              (any-overlap? {:from 1 :to 2} [{:from 2 :to 3} {:from 8 :to 9}])))
  (test/is (= [{:from 2 :to 3} {:from 8 :to 9}]
              (any-overlap? {:from 1 :to 8} [{:from 2 :to 3} {:from 8 :to 9}]))))

(defn- merge-ranges [{from1 :from to1 :to} {from2 :from to2 :to}]
  {:from (min from1 from2)
   :to   (max to1 to2)})

(defn- merge-or-conj-ranges [{:keys [coll] :as m} r]
  (if-let [overlapping-ranges (any-overlap? r coll)]
    (->
     (assoc m :overlap-combined? true)
     (update :coll #(apply disj % overlapping-ranges))
     (update :coll conj (reduce merge-ranges r overlapping-ranges)))
    (update m :coll conj r)))

(defn- combine-overlapping-ranges [{:keys [ranges]}]
  (reduce merge-or-conj-ranges {:coll #{} :overlap-combined? false} ranges))

(defn- deep-combine [{:keys [ranges]}]
  (loop [{:keys [coll overlap-combined?]} {:coll              ranges
                                           :overlap-combined? true}]
    (if (not overlap-combined?)
      coll
      (recur (combine-overlapping-ranges {:ranges coll})))))

(test/deftest deep-combine-test
  (test/is (= #{{:from 10, :to 20} {:from 3, :to 5}}
              (-> "day5/test.txt" read-data deep-combine)))
  (test/is (= 97 ;; Should find total of 97 combined ranges
              (-> "day5/input.txt" read-data deep-combine count))))

(defn- range-difference [{:keys [from to]}]
  (- (inc to) from))

(test/deftest day5part2-test
  (letfn [(day5part2 [filename]
            (->> filename read-data deep-combine (map range-difference) (reduce +') time))]
    (test/is (= 14 (day5part2 "day5/test.txt")))
    (test/is (= 355555479253787 (day5part2 "day5/input.txt")))))
