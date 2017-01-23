(ns clojure-dectree.core-test
  (:require [clojure.test :refer :all]
            [clojure-dectree.core :refer :all]))
(def my-data [[2.771244718		1.784783929		0]
              [1.728571309		1.169761413		0]
              [3.678319846		2.81281357		0]
              [3.961043357		2.61995032		0]
              [2.999208922		2.209014212		0]
              [7.497545867		3.162953546		1]
              [9.00220326		3.339047188		1]
              [7.444542326		0.476683375		1]
              [10.12493903		3.234550982		1]
              [6.642287351		3.319983761		1]])  

;(def tree (split (find-best-split my-data) 3 1 1))

;(def splits (splits-list tree))

(deftest test-proportion
  (testing "aaa"
    (is (= (proportion [[99 0] [99 1]] 1) (/ 1 2)))))

(deftest test-gini-zero
  (testing "aaa"
    (is (= (gini-index [[1 2 3 0] [1 2 3 0] [1 2 3 0]]) 0))))

(deftest test-gini-half
  (testing "aaa"
    (is (= (gini-index [[1 2 3 0] [1 2 3 1]]) 1/2))))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))
