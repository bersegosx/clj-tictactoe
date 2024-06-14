(ns berseg.tictactoe-test
  (:require [clojure.test :refer :all]
            [berseg.tictactoe :refer :all]))

(deftest next-mark-test
  (testing
   (are [value expected] (= expected (get-next-mark value))
     ["X" "0"] "X"
     [] "X"
     ["X" "0" "X"] "0")))

(deftest in-range-test
  (testing
   (are [expected value range-v]
        (= expected (in-range? value range-v))
     true 3 [1 3]
     false 4 [1 3])))

(deftest number-to-index-test
  (testing
   (are [value expected]
        (= expected (number-to-index value))
     2 [0 1]
     6 [1 2]
     8 [2 1])))

(deftest has-winner-mark-test
  (testing
   (are [board expected]
        (= expected (has-winner-mark? board "X"))
     [["X" "X" "X"]
      [\_ \_ \_]
      [\_ \_ \_]] true

     [["X" \_ \_]
      ["X" \_ \_]
      ["X" \_ \_]] true

     [["X" \_ \_]
      [\_ "X" \_]
      [\_ \_ "X"]] true

     [[\_ \_ "X"]
      [\_ "X" \_]
      ["X" \_ \_]] true

     [["X" \_ \_]
      [\_ "0" \_]
      [\_ \_ "X"]] false

     [["X" "0" "X"]
      [\_ \_ \_]
      [\_ \_ \_]] false)))