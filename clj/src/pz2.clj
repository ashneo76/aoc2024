(ns pz2
  [:require [clojure.test :as t]]
  [:use cadr])

(def test-reports "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

;; utility
(defn parse-report [report-str]
  (as-> report-str x
    (clojure.string/split x #"\n")
    (map (fn [row]
           (map #(Integer/parseInt %)
                (clojure.string/split row #" +")))
         x)))
;; (parse-report test-reports)

(defn lst->steps [lst]
  (loop [acc []
         clst lst]
    #_(println acc " " clst)
    (if (or (empty? clst) (empty? (cdr clst)))
      (do
        #_(println "/")
        acc)
      (do
        (recur (conj acc (abs (- (first clst) (cadr clst)))) (cdr clst))))))

;; rules
(defn rule1 [lst]
  (cond (< (first lst) (second lst)) (= lst (sort lst))
        :else (= lst (reverse (sort lst)))))
(filter rule1 (parse-report test-reports))

(defn rule2 [lst]
  (let [filtered-lst (filter #(or (< % 1) (> % 3)) (lst->steps lst))]
    #_(println filtered-lst)
    (or (empty? filtered-lst))))

;; actual test
(defn valid-reports [report-str rule1 rule2]
  (filter rule2 (filter rule1 report-str)))

(defn invalid-reports [report-str rule1 rule2]
  (set (concat (filter #(not (rule1 %)) report-str) (filter #(not (rule2 %)) report-str))))

;; test
(def lst [9 7 6 5 4 1])
(def step-lst (lst->steps lst))

(t/is (= 2
         (count
          (valid-reports (->> test-reports
                              parse-report)
                         rule1
                         rule2))))

;; actual
;; print count of valid reports
(println (count (valid-reports (parse-report (slurp "data/2.csv"))
                               rule1 rul2)))

;; 2.1
;; -- utils
;; generate lists with 1 element dropped
(defn drop-1-lst [lst]
  (let [lst-cnt (- (count lst) 1)]
    (for [x (range -1 lst-cnt)]
      (flatten (concat (subvec (vec lst) 0 (+ 1 x))
                       (cond (< x lst-cnt)
                             (subvec (vec lst) (+ 2 x))))))))

;; -- debugging
(def invalid-report-test [[1 3 2 4 5]])
(valid-reports invalid-report-test rule1 rule2)
(invalid-reports invalid-report-test rule1 rule2)

;; actual
;; check if any invalid reports are actually valid due to the problem dampener
(defn valid-reports-2-1 [report-lst rule1 rule2]
  ;; filter out invalid reports that didn't return any valid reports
  ;; even after dropping 1 element
  (filter
   #(not (empty?
          ;; find any valid reports in 1 element dropped list
          (valid-reports
           ;; generate a list of reports with 1 element dropped
           (drop-1-lst %)
           rule1 rule2)))
   ;; generate list of invalid reports
   (invalid-reports report-lst rule1 rule2)))

(defn total-valid-reports-2-1 [report-str rule1 rule2]
  (let [report-lst (->> report-str
                        parse-report)]
    ;; combine all valid reports and invalid reports that are now valid due to problem dampener
    (concat (valid-reports report-lst rule1 rule2)
            (valid-reports-2-1 report-lst rule1 rule2))))
(println (count (total-valid-reports-2-1 (slurp "data/2.csv") rule1 rule2)))
