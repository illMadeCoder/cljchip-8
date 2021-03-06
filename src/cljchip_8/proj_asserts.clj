(ns cljchip-8.proj-asserts)

(defn- assert-message-arg-is-of-type
  [x]
  (str "arg is: " x " of type: " (type x)))

;;Numerics
(defn assert-is-number
  [x]
  (assert (number? x) (assert-message-arg-is-of-type x)))

(defn assert-is-whole-number
  [x]
  (assert-is-number x)
  (assert (= (mod x 1) 0) (assert-message-arg-is-of-type x)))

(defn assert-is-in-range
  [start x end]
  (assert-is-number start)
  (assert-is-number end)
  (assert-is-number x)
  (assert (<= start x end) (assert-message-arg-is-of-type x)))

;;Binaries
(defn assert-is-bit
  [x]
  (assert (or (= x 0) (= x 1)) (assert-message-arg-is-of-type x)))

(defn assert-is-nibble
  [x]
  (assert (= (count x) 4) (assert-message-arg-is-of-type x)))

(defn assert-is-byte
  [x]
  (assert (= (count x) 8) (assert-message-arg-is-of-type x)))

(defn assert-is-hexstring-nibble
  [x]
  (assert (re-matches #"[0-9a-fA-F]" x) (assert-message-arg-is-of-type x)))

(defn assert-is-hexstring-byte
  [x]
  (assert (re-matches #"[0-9a-fA-F]{2}" x) (assert-message-arg-is-of-type x)))

;;Memory
(defn assert-is-memory-loc
  [x]
  (assert-is-byte x))

(defn assert-is-memory
  [x]
  (assert vector x)
  (loop [i 0]
    (if (< i (count x))
      (do
        (assert-is-memory-loc (nth x i))
        (recur (inc i))))))

;;Rom
(defn assert-is-file
  [x]
  (assert instance? (java.io.File x)))

(defn assert-is-byte-array
  [x]
  (assert (bytes? x)))

(defn assert-is-rom
  [x]
  (assert-is-memory x))
