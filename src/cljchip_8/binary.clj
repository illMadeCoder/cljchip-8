(ns cljchip-8.binary)

;;b = bit
;;bin = binary

;;pre-conditions
(def value [])
(def bit-conditions [#(= 1 %) #(= 0 %)])
(def nibble-conditions [#(= (count %) 4)])
(def byte-conditions [#(= (count %) 8)])


;;Transformations
(defn value->b
  "A b is a 1 or 0, can translate from truthy or falsy, or not 1 or 0 respectively"
  [b]
  (let [binary-zero 0 binary-one 1]
    (cond
      (= b 0) binary-zero
      (false? b) binary-zero
      (nil? b) binary-zero
      :else binary-one)))

(defn value+index->bit
  "Determines the bit per given index over a java-byte"
  [value index]
  (let [least-sig 1]
    (-> value
        (clojure.core/bit-shift-right index)
        (clojure.core/bit-and least-sig)
        (value->b))))

(defn binary-index->bit
  [binary index]
  (nth binary index))

(defn value->binary
  "A binary is a more generic definition than a nibble, byte, word;
  where a binary is simply a vector of bits of any size."
  [value width]
  (loop [i 0 binary '()]
    (if (< i width)
      (recur (inc i) (conj binary (value+index->bit value i)))
      (vec binary))))

(defn value->byte
  "A java byte is a primative 8-bit signed two's complement integer
  and is a leaky abstraction for cljchip. Any java-byte should convert
  to a cljchip-8 byte to be used in the project A cljchip-8 byte is"
  [value]
  (let [byte-width 8]
    (value->binary value byte-width)))

(defn value->nibble
  "A java byte is a primative 8-bit signed two's complement integer
  and is a leaky abstraction for cljchip. Any java-byte should convert
  to a cljchip-8 byte to be used in the project A cljchip-8 byte is"
  [value]
  (let [nibble-width 4]
    (value->binary value nibble-width)))

(defn byte->nibble+nibble
  "first is left byte side, second is right byte side"
  [byte]
  (map vec (partition 4 byte)))

(defn nibble+nibble->byte
  [nibble-l nibble-r]
  (vec (concat nibble-l nibble-r)))

(defn byte+byte->word
  [byte-l byte-r]
  (vec (concat byte-l byte-r)))

(defn binary->string
  "The inverse of value->binary, loses any data cutoff in the binary's width"
  [binary]
  (reduce #(str %1 %2) "" binary))

(defn exp
  "exponent of x^n (int n only), with tail recursion and O(logn)"
   [x n]
   (if (< n 0)
     (/ 1 (exp x (- n)))
     (loop [acc 1
            base x
            pow n]
       (if (= pow 0)
         acc                           
         (if (even? pow)
           (recur acc (* base base) (/ pow 2))
           (recur  (* acc base) base (dec pow)))))))

(defn binary->value
  [binary]
  (reduce-kv #(+ %1 (* %3 (exp 2 %2))) 0 (vec (reverse binary))))

;;Limited, assumes a binary of width mod 4 is 0
(defn binary->hexstring
  [binary]
  (format (str "%0" (quot (count binary) 4) "x") (binary->value binary)))

(defn hexstring->binary
  [hexstring]
  "return a byte value from a hexstring representation"
  (vec (apply concat (map #(value->binary (Integer/parseInt (str %) 16) 4) hexstring))))

;;Operations
;;bit operations
(defn b-and
  [bit-l bit-r]
  (if (and (= bit-l 1) (= bit-r 1)) 1 0))

(defn b-or
  [bit-l bit-r]
  (if (or (= bit-l 1) (= bit-r 1)) 1 0))

(defn b-xor
  [bit-l bit-r]
  (cond
    (and (= bit-l 1) (= bit-r 0)) 1
    (and (= bit-l 0) (= bit-r 1)) 1
    :else 0))

(defn b-not
  [bit]
  (if (= bit 0)
    1
    0))

;;binary operations

(defn bin-most-sig
  [binary]
  (first binary))

(defn bin-least-sig
  [binary]
  (last binary))

(defn bin-shiftl
  [binary]
  (let [m-s-b (bin-most-sig binary)]
    (conj (vec (rest binary)) m-s-b)))

(defn bin-shiftr
  [binary]
  (let [l-s-b (bin-least-sig binary)]
    (conj (butlast binary) l-s-b)))

(defn bin-mask-op
  [op binary-l binary-r]
  (loop [i 0
         width (max (count binary-l) (count binary-r))
         binary-return []]
    (if (< i width)
      (recur (inc i)
             width
             (conj binary-return (value->b (op (nth binary-l i) (nth binary-r i)))))
      binary-return)))

(defn bin-and [bin-l bin-r] (bin-mask-op b-and bin-l bin-r))
(defn bin-or [bin-l bin-r] (bin-mask-op b-or bin-l bin-r))
(defn bin-xor [bin-l bin-r] (bin-mask-op b-xor bin-l bin-r))
(defn bin-flip [bin] (bin-xor bin (value->binary -1 (count bin))))

;;binary numerics

(defn bin-add
  [binary value]
  (value->binary (+ (binary->value binary) value) (count binary)))

(defn bin-sub
  [binary value]
  (value->binary (- (binary->value binary) value) (count binary)))

(defn bin-inc
  [binary]
  (bin-add binary 1))

(defn bin-dec
  [binary]
  (bin-sub binary 1))

(defn bin-mult
  [binary value]
  (value->binary (* (binary->value binary) value) (count binary)))

(defn bin-is-zero?
  [binary]
  (zero? (binary->value binary)))

(defn bin-is-neg?
  [binary]
  (neg? (binary->value binary)))

(defn bin-is-pos?
  [binary]
  (pos? (binary->value binary)))
