(ns crashmas25.day2
  (:require [clojure.edn :as edn]
            [clojure.test :as test]
            [clojure.string :as str]
            [crashmas25.day1 :refer [read-input]]))

(defn- parse-range-input [s]
  (->> (str/split s #"-") (map (comp bigint edn/read-string))))

(test/deftest parse-range-input-test
  (test/is
    (= [11N 22N 115N 1012N 1188511890N 222224N 1698528N 446449N 38593862N 565659N 824824827N 2121212124N]
            (->> "day2/test.txt" read-input first parse-range-input))))

(defn- silly-id* [{:keys [digit-cnt n-digits digit-str n-partition]}]
  (when (= n-digits digit-cnt)
    (let [n (or n-partition (-> n-digits (/ 2) int))]
      (apply = (partition n digit-str)))))

(defn- silly-id? [n]
  (-> n integer? assert)
  (let [digit-str (str n)
        digit-cnt (count digit-str)
        props {:digit-str digit-str :digit-cnt digit-cnt}]
    (when (even? digit-cnt)
      (or (reduce = digit-str)
          (silly-id* (assoc props :n-digits 4))
          (silly-id* (assoc props :n-digits 6))
          (silly-id* (assoc props :n-digits 8))
          (silly-id* (assoc props :n-digits 10))))))

(defn- silly-patterns [f coll]
  (set (filter (memoize f) coll)))

(defn- floors [floor ceiling]
  (let [cnt (-> floor str count)
        divisor (if (even? cnt) 3 2)
        div (Math/pow 10 (-> cnt (/ divisor) bigint))
        a   (-> floor (/ div) bigint)
        b   (-> ceiling (/ div) bigint)]
    (->> (range a (inc b)) (map (comp bigint (partial * div))))))


(defn- list-silly-codes-for [{:keys [s floor ceiling]}]
  (let [str-cnt (count s)]
    (->> (for [i (->> (range 1 (-> str-cnt (/ 2) int inc))
                      (filter #(-> str-cnt (/ %) int?)))]
           (->> (take i s)
                cycle
                (take str-cnt)
                (apply str)
                edn/read-string
                bigint))
         (filter #(<= floor % ceiling))
         set)))

(defn- silly-codes [floor ceiling]
  (->> (floors floor ceiling)
       (mapcat #(list-silly-codes-for {:floor (bigint %) :ceiling ceiling :s (str %)}))
       (into #{})))

(comment
  (silly-codes 90 120)
  (floors 565653 565660)
  (silly-codes 565653 565660))

(test/deftest silly-patterns-test
  (test/is (= #{11 22} (silly-patterns silly-id? (range 11 23))))
  (test/is (= #{99} (silly-patterns silly-id? (range 95 116))))
  (test/is (= #{1010} (silly-patterns silly-id? (range 998 1013))))
  (test/is (= #{1188511885} (silly-patterns silly-id? (range 1188511880 1188511891))))
  (test/is (= #{222222} (silly-patterns silly-id? (range 222220 222225))))
  (test/is (= #{} (silly-patterns silly-id? (range 1698522 1698529))))
  (test/is (= #{446446} (silly-patterns silly-id? (range 446443 446450))))
  (test/is (= #{38593859} (silly-patterns silly-id? (range 38593856 38593863))))
  (test/is (= #{} (silly-patterns silly-id? (range 565653 565659))))
  (test/is (= #{} (silly-patterns silly-id? (range 824824821 824824828))))
  (test/is (= #{} (silly-patterns silly-id? (range 2121212118 2121212125)))))

(test/deftest silly-codes-test
  (test/is (= #{11 22} (silly-codes 11 22)))
  (test/is (= #{99 111} (silly-codes 95 116)))
  (test/is (= #{999 1010} (silly-codes 998 1013)))
  (test/is (= #{1188511885} (silly-codes 1188511880 1188511891)))
  (test/is (= #{222222} (silly-codes 222220 222225)))
  (test/is (= #{} (silly-codes 1698522 1698529)))
  (test/is (= #{446446} (silly-codes 446443 446450)))
  (test/is (= #{38593859} (silly-codes 38593856 38593863)))
  (test/is (= #{565656} (silly-codes 565653 565660)))
  (test/is (= #{824824824} (silly-codes 824824821 824824828)))
  (test/is (= #{2121212121} (silly-codes 2121212118 2121212125))))

(test/deftest day2-test
  (letfn [(day2part1 [filename f]
            (println "Running day2part1:" filename)
            (->> filename
                 read-input
                 (mapcat #(str/split % #","))
                 (map parse-range-input)
                 (map (juxt first (comp inc second)))
                 (map (partial apply range))
                 (mapcat (partial silly-patterns f))
                 (reduce + 0)
                 time))
          (day2part2 [filename]
            (println "Running day2part2:" filename)
            (->> filename
                 read-input
                 (mapcat #(str/split % #","))
                 (map parse-range-input)
                 (mapcat (partial apply silly-codes))
                 (reduce +' 0)
                 time))]
    (test/is (-> "day2/test.txt" (day2part1 silly-id?) (= 1227775554)))
    (test/is (-> "day2/input.txt" (day2part1 silly-id?) (= 30608905813)))
    (test/is (-> "day2/test.txt" day2part2  (= 4174379265)))
    (test/is (-> "day2/input.txt" day2part2 (= 31898925685)))))
