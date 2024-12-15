(ns pz1
  [:require [clojure.test :as t]])

;; -- problem 1
;; util
(defn parse-csv [raw]
  (let [rows (clojure.string/split raw #"\n")
        tuples (map #(clojure.string/split % #" +") rows)
        left (sort (map #(Integer/parseInt (first %)) tuples))
        right (sort (map #(Integer/parseInt (second %)) tuples))
        zipped (map vector left right)]
    zipped))

;; util test
(def test2 "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")
;; (parse-csv test2)
;; (def parsed-rows (parse-csv (slurp "data/1.csv")))

;; actual solution
(defn pz1 [parsed-rows]
  (let [distances (map #(abs (- (first %) (second %))) parsed-rows)]
    (reduce + distances)))

;; test
(def test-data "3   4
4   3
2   5
1   3
3   9
3   3")
(def raw test-data)
(t/is (= 11 (pz1 (parse-csv test-data))))

;; apply to actual data
(println (pz1 (parse-csv (slurp "data/1.csv"))))

;; -- problem 2
;; util
(defn occurrences [itm lst]
  (count (filter #(= % itm) lst)))

;; test occurrences
(t/is (= 0 (occurrences 3 [1 2 5])))
(t/is (= 2 (occurrences 3 [1 2 5 3 4 3])))

;; actual solution
(defn pz2 [parsed-rows]
  (let [left (map first parsed-rows)
        right (map second parsed-rows)
        freq-fn (fn [itm lst] (* itm (occurrences itm lst)))
        freq (map #(freq-fn % right) left)]
    (reduce + freq)))
(t/is (= 31 (pz2 (parse-csv test-data))))

;; apply to actual data
(println (pz2 (parse-csv (slurp "data/1.csv"))))
