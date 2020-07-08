(ns t1.core-test
  (:require [clojure.test :refer :all]
            [t1.core :refer :all]))

(deftest evaluate-test
  (testing "operations."
    (is (= (evaluate {} '(+ 1 1)) 2))
    (is (= (evaluate {} '(- 2 1)) 1))
    (is (= (evaluate {} '(* 2 2)) 4))
    (is (= (evaluate {} '(/ 6 3)) 2))
    (is (= (evaluate {} '(abs -3)) 3))
    (is (= (evaluate {} '(power 2 4)) 16.0)))

  (testing "variables resolution"
    (is (= (evaluate {:x 10} '(* x x)) 100))
    (is (= (evaluate {:x 2 :y 3} '(power x (* x y))) 64.0)))

  (testing "Nested operations"
    (is (= (evaluate {} '(/ 6 (+ 1 2))) 2))
    (is (= (evaluate {} '(power (+ 2 2) (abs (- 4 5)))) 4.0)))

  (testing "Exceptions"
    (is (thrown? Exception (evaluate {} '(- 1 (+) 2))))
    (is (thrown? Exception (evaluate {} '(power "1" 2))))
    (is (thrown? Exception (evaluate {} '(power 1 2 3))))
    (is (thrown? Exception (evaluate {} '(pow 1 2))))
    (is (thrown? Exception (evaluate {} '(abs 1 2 3))))))

(deftest optimize-test
  (testing "addition"
    (is (= (optimize '(+ 1 0 2)) '(+ 1 2)))
    (is (= (optimize '(+ x 0)) 'x))
    (is (= (optimize '(* x (+ 2 0))) '(* x 2))))
  (testing "multiplication"
    (is (= (optimize '(* x 1)) 'x))
    (is (= (optimize '(* x (+ 1 (/ y 1)))) '(* x (+ 1 y))))
    (is (= (optimize '(* x (+ 1 (* y 0 1 2 (+ 4 3) 4 5)))) 'x))
    (is (= (optimize '(/ 1 y 1)) '(/ 1 y))))
  (testing "power and abs"
    (is (= (optimize '(abs (abs (abs x)))) '(abs x)))
    (is (= (optimize '(power (abs x) 2)) '(power x 2)))
    (is (= (optimize '(power x 0)) 1))
    (is (= (optimize '(power x 1)) 'x))))

(deftest ->javascript-test
  (testing "->javascript"
    (is (= (->javascript "example" '(+ 1 x y (* 4 z) 6))
           "function example(x, y, z) { return (1 + x + y + (4 * z) + 6); }"))
    (is (= (->javascript "example" '(+ 1 x y (abs (* 4 z)) 6))
           "function example(x, y, z) { return (1 + x + y + Math.abs((4 * z)) + 6); }"))
    (is (= (->javascript "example" '(+ 1 x y (power 4 (* z z)) 6))
           "function example(x, y, z) { return (1 + x + y + Math.pow(4, (z * z)) + 6); }"))))