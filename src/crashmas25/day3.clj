(ns crashmas25.day3
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :as test]
            [crashmas25.day1 :refer [read-input]]))

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

;;;;; Sandbox for part 2

(defn- skip-lowest
  [v]
  (->>
   (-> v count range)
   (reduce
    (fn [coll n]
      (let [skip-one   (assoc v n nil)
            next-value (->> skip-one (reduce str) edn/read-string)]
        (conj coll next-value)))
    nil)
   (reduce max)))

(defn- take-some
  [coll n]
  (let [v (vec (take-last (inc n) coll))
        before (drop-last (inc n) coll)]
    {:before before
     :skipped (skip-lowest v)}))

(defn- find-highest-joltage
  [n s]
  (let [coll (map #(-> % str edn/read-string) s)]
    (loop [{:keys [before skipped]} (take-some coll n)]
      (if-not (seq before)
        skipped
        (recur
         (take-some
          (concat before
                  (->> skipped str seq (map (comp edn/read-string str))))
          n))))))

(test/deftest find-highest-joltage-test
  (test/is (= 66 (find-highest-joltage 2 "46606")))
  (test/is (= 965 (find-highest-joltage 3 "19262154")))
  (test/is (= 9756 (find-highest-joltage 4 "59657356")))
  (test/is (= 57245 (find-highest-joltage 5 "5121145357245")))
  (test/is (= 55357245 (find-highest-joltage 8 "5121145357245")))
  (test/is (= 987654321111 (->> "day3/test.txt" read-input first (find-highest-joltage 12))))
  (test/is (= 811111111119 (->> "day3/test.txt" read-input second (find-highest-joltage 12))))
  (test/is (= 888911112111 (->> "day3/test.txt" read-input last (find-highest-joltage 12)))))

(defn- day3part2 [filename]
  (->> filename
       read-input
       (map (partial find-highest-joltage 12))
       (reduce +' 0)
       time))

(test/deftest day2part2-test
  (test/is (= 3121910778619 (-> "day3/test.txt" day3part2)))
  (test/is (= 173848577117276 (-> "day3/input.txt" day3part2))))
