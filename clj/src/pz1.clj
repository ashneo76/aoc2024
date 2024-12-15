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

;; generate a difference of the two elements of each row and distance is always an abs
(defn distance [row]
  (abs (- (first row) (second row))))

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
  ;; get distances for all rows
  (let [distances (map distance parsed-rows)]
    ;; take a sum of all distances
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
;; get the count of item in a list
(defn occurrences [itm lst]
  (count (filter #(= % itm) lst)))

;; the frequence function is itm * number of occurrences of item in the list
(defn freq-fn [itm lst]
  (* itm (occurrences itm lst)))

;; test occurrences
(t/is (= 0 (occurrences 3 [1 2 5])))
(t/is (= 2 (occurrences 3 [1 2 5 3 4 3])))

;; actual solution
(defn pz2 [parsed-rows]
  (let [left (map first parsed-rows) ;; left list
        right (map second parsed-rows) ;; right list

        ;; get frequency for each item in the left list
        freq (map (fn [x] (freq-fn x right)) left)]

    ;; sum of all frequencies
    (reduce + freq)))

(t/is (= 31 (pz2 (parse-csv test-data))))

;; apply to actual data
(println (pz2 (parse-csv (slurp "data/1.csv"))))
