(ns pz1
  [:require [clojure.test :as t]])

(defn parse-csv [raw]
  (let [rows (clojure.string/split raw #"\n")
        tuples (map #(clojure.string/split % #" +") rows)
        left (sort (map #(Integer/parseInt (first %)) tuples))
        right (sort (map #(Integer/parseInt (second %)) tuples))
        zipped (map vector left right)]
    zipped))

(defn pz1 [parsed-rows]
  (let [distances (map #(abs (- (first %) (second %))) parsed-rows)]
    (reduce + distances)))

(def test-data "3   4
4   3
2   5
1   3
3   9
3   3")
(def raw test-data)
(t/is (= 11 (pz1 (parse-csv test-data))))

(println (pz1 (parse-csv (slurp "data/1.csv"))))

(defn occurrences [itm lst]
  (count (filter #(= % itm) lst)))
(t/is (= 0 (occurrences 3 [1 2 5])))
(t/is (= 2 (occurrences 3 [1 2 5 3 4 3])))

;; (def parsed-rows (parse-csv (slurp "data/1.csv")))
(defn pz2 [parsed-rows]
  (let [left (map first parsed-rows)
        right (map second parsed-rows)
        freq-fn (fn [itm lst] (* itm (occurrences itm lst)))
        freq (map #(freq-fn % right) left)]
    (reduce + freq)))
(t/is (= 31 (pz2 (parse-csv test-data))))

(println (pz2 (parse-csv (slurp "data/1.csv"))))


(def test2 "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")
(parse-csv test2)
