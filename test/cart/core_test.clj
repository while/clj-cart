(ns cart.core-test
  (:require [clojure.test :refer :all]
            [cart.core :refer :all]))


; Test find-split on perfect set w all ones
(deftest nosplit-test-all-one
  (testing "Test find-split on perfect set w all 1"
    (is (= (find-split [1 1 1 1 1])
           {:gini 0 :idx -1}))))


; Test find-split on perfect set w all zeros
(deftest nosplit-test-all-zero
  (testing "Test find-split on perfect set w all 0"
    (is (= (find-split [0 0 0 0 0])
           {:gini 0 :idx -1}))))


; Test find-split on perfectly sepparable set
(deftest easysplit-test
  (testing "Test find-split on sepparable set"
    (is (= (find-split [1 1 1 0 0])
           {:gini 0 :idx 3}))))


; Test find-split on perfectly sepparable set mirrored
(deftest easysplit-test-mirror
  (testing "Test find-split on sepparable set mirrored"
    (is (= (find-split [0 0 0 1 1])
           {:gini 0 :idx 3}))))


; Test find-split on perfectly sepparable set
(deftest hardsplit-test
  (testing "Test find-split on non sepparable set"
    (is (= (find-split [1 0 1 0 1])
           {:gini 12/25 :idx -1}))))


;;;; Read data prom file and parse into vector of maps
;;(def data
;;  (rest (map (fn [line] 
;;               (into {} 
;;                     (map #(vector %1 (read-string %2))
;;                          [:x1 :x2 :y] (split line #",")))) 
;;             (split (slurp "resources/data.csv") #"\n"))))
