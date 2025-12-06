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
