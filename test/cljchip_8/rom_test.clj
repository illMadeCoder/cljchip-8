;; (ns cljchip-8.rom-test
;;   (:require [cljchip-8.rom :refer :all]
;;             [clojure.test :refer :all]
;;             [clojure.java.io :refer :all]))

(def samples-file-path "test/cljchip_8/samples/")
(def empty-file (file (str samples-file-path "empty")))
(def pong-file (file (str samples-file-path "pong")))
(def a-byte-file (file (str samples-file-path "a-byte")))

(deftest read-rom
  (is (= (alength (read-file->byte-array empty-file)) 0)
      "Empty rom returns 0 binary data")
  (is (= (alength (read-file->byte-array a-byte-file)) 1)
      "A byte rom file returns = 1 binary data")
  (is (= (alength (read-file->byte-array pong-file)) (.length pong-file))
      "Non-empty rom file returns > 0 binary data"))
