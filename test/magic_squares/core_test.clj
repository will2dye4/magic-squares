(ns magic-squares.core-test
  (:require [clojure.test :refer :all]
            [magic-squares.core :refer :all]))

(deftest a-test
  (testing "Number of Parker squares"
    (is (= 7 (count parker-squares)))))
