(ns cart.core-test
  (:require [clojure.test :refer :all]
            [cart.core :refer :all]))


;; Test one? function
(deftest test-one?
  (testing "Test double 1.0"
    (is (one? (double 1.0))))
  (testing "Test int 1"
    (is (one? (int 1))))
  (testing "Test not one? 1"
    (is (not (one? 0))))
  (testing "Test 1/1"
    (is (one? 1/1))))


;; Test avg function 
(deftest test-avg
  (testing "Test avg of zero"
    (is (zero? (avg [0]))))
  (testing "Test avg of [0 1 0 1]"
    (is (= (avg [0 1 0 1]) 1/2)))
  (testing "Test avg of [0 0.5 -0.5 0]"
    (is (zero? (avg [0 0.5 -0.5 0])))))


;; Test gini impurity function
(deftest test-gini
  (testing "Gini on single 1"
    (is (zero? (gini 1))))
  (testing "Gini on single 0"
    (is (zero? (gini 0))))
  (testing "Gini on two-class (0 1)"
    (is (zero? (gini 0 1))))
  (testing "Gini on two-class (1/2 1/2)"
    (is (= (gini 1/2 1/2) 1/2)))
  (testing "Gini on three-class 1/3"
    (is (= (gini 1/3 1/3 1/3) 2/3)))
  (testing "Gini on three-class 1"
    (is (= (gini 0 0 1) 0))))


;; Test find-split
(deftest test-find-split
  (testing "Test find-split on perfect set w all 1"
    (is (= (find-split [1 1 1 1 1])
           {:gini 0 :idx -1})))
  (testing "Test find-split on perfect set w all 0"
    (is (= (find-split [0 0 0 0 0])
           {:gini 0 :idx -1})))
  (testing "Test find-split on sepparable set"
    (is (= (find-split [1 1 1 0 0])
           {:gini 0 :idx 3})))
  (testing "Test find-split on sepparable set mirrored"
    (is (= (find-split [0 0 0 1 1])
           {:gini 0 :idx 3})))
  (testing "Test find-split on non sepparable set"
    (is (= (find-split [1 0 1 0 1])
           {:gini 12/25 :idx -1}))))


