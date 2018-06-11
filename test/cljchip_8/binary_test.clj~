(ns cljchip-8.binary-test
  (:require [clojure.test :refer :all]
            [cljchip-8.binary :refer :all]))

(def binary-zero [0])
(def binary-one [1])
(def nibble-zeros [0 0 0 0])
(def nibble-ones [1 1 1 1])
(def byte-zeros [0 0 0 0 0 0 0 0])
(def byte-ones [1 1 1 1 1 1 1 1])
(def byte-mixed [0 1 0 1 1 0 1 0])
(def word-zeros (concat byte-zeros byte-zeros))
(def word-ones (concat byte-ones byte-ones))

(deftest value->bit-test
  (let [f value->bit]
    (is (= (f 1) 1))
    (is (= (f true) 1))
    (is (= (f 'a) 1))
    (is (= (f -1) 1))
    (is (= (f false) 0))
    (is (= (f 0) 0))
    (is (= (f nil) 0))))

(deftest value-index->bit-test
  (let [f value-index->bit]
    (is (= 1 (f 1 0)))
    (is (= 0 (f 0 0)))
    (is (= 0 (f 2 0)))
    (is (= 1 (f 2 1)))))

(deftest binary-index->bit-test
  (let [f binary-index->bit]
    (is (= 1 (f [1] 0)))
    (is (= 1 (f [0 1] 1)))
    (is (= 1 (f [1 0] 0)))
    (is (= 0 (f [0 1 1 1 1 1 1 1] 0)))))

(deftest value->binary-test
  (let [f value->binary]
    (is (= binary-zero (value->binary 0 1)))
    (is (= binary-one (value->binary 255 1)))
    (is (= binary-one (value->binary -1 1)))))

(deftest value->byte-test
  (let [f value->byte]
    (is (= byte-zeros (f 0)))
    (is (= byte-ones (f 255)))
    (is (= byte-ones (f -1)))))

(deftest value->nibble-test
  (let [f value->nibble]
    (is (= nibble-zeros (f 0)))
    (is (= nibble-ones (f 15)))
    (is (= nibble-ones (f -1)))))

(deftest byte->nibble+nibble-test
  (let [f byte->nibble+nibble]
    (is (= (list nibble-zeros nibble-zeros) (f byte-zeros)))
    (is (= (list nibble-ones nibble-ones) (f byte-ones)))))

(deftest nibble+nibble->byte-test
  (let [f nibble+nibble->byte]
    (is (= byte-zeros (f nibble-zeros nibble-zeros)))
    (is (= byte-ones (f nibble-ones nibble-ones)))))

(deftest byte+byte->word-test
  (let [f byte+byte->word]
    (is (= word-zeros (f byte-zeros byte-zeros)))
    (is (= word-ones (f byte-ones byte-ones)))))

(deftest binary->string-test
  (let [f binary->string]
    (is (= "0" (f binary-zero)))
    (is (= "1" (f binary-one)))
    (is (= "01011010" (f byte-mixed)))))

(deftest exp-test
  (let [f exp]
    (is (= 1 (f 0 0)))
    (is (= 2 (f 2 1)))
    (is (= 4 (f 2 2)))))

(deftest binary->value-test
  (let [f binary->value]
    (is (= 0 (f binary-zero)))
    (is (= 1 (f binary-one)))
    (is (= 255 (f byte-ones)))))

(deftest binary->hexstring-test
  (let [f binary->hexstring]
    (is (= "0" (f nibble-zeros)))
    (is (= "f" (f nibble-ones)))
    (is (= "00" (f byte-zeros)))
    (is (= "ffff" (f word-ones)))))

(deftest b-and-test
  (let [f b-and]
    (is (= 0 (f 0 1)))
    (is (= 1 (f 1 1)))))

(deftest b-or-test
  (let [f b-or]
    (is (= 1 (f 0 1)))
    (is (= 1 (f 1 1)))))

(deftest b-xor-test
  (let [f b-xor]
    (is (= 1 (f 0 1)))
    (is (= 0 (f 1 1)))))

(deftest b-not-test
  (let [f b-not]
    (is (= 1 (f 0)))
    (is (= 0 (f 1)))))

(deftest bin-most-sig-test
  (let [f bin-most-sig]
    (is (= 1 (f [1 0 0 0])))
    (is (= 0 (f [0 1 1 1])))))

(deftest bin-least-sig-test
  (let [f bin-least-sig]
    (is (= 0 (f [1 0 0 0])))
    (is (= 1 (f [0 1 1 1])))))

(deftest bin-shiftl-test
  (let [f bin-shiftl]
    (is (= [1 0 0] (f [0 1 0])))))

(deftest bin-shiftr-test
  (let [f bin-shiftr]
    (is (= [0 0 1] (f [0 1 0])))))

(deftest bin-and-test
  (let [f bin-and]
    (is (= byte-zeros (f byte-zeros byte-ones)))
    (is (= byte-ones (f byte-ones byte-ones)))))

(deftest bin-or-test
  (let [f bin-or]
    (is (= byte-zeros (f byte-zeros byte-zeros)))
    (is (= byte-ones (f byte-zeros byte-ones)))))

(deftest bin-xor-test
  (let [f bin-xor]
    (is (= byte-ones (f byte-zeros byte-ones)))
    (is (= byte-zeros (f byte-ones byte-ones)))))

(deftest bin-flip-test
  (let [f bin-flip]
    (is (= byte-zeros (f byte-ones)))
    (is (= byte-ones (f byte-zeros)))))
