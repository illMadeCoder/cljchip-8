;;Opcodes reflect CHIP-8 Opcode table https://en.wikipedia.org/wiki/CHIP-8
;;Defined as an array of 2 tuples (opcode-capture-template, opcode-lambda)
;;Where opcode-capture-template is defined as a regex who will match on a givin opcode, if it does match it will return the opcode-lambda arguments.
;;and opcode-lambda is defined as some function that works on the chip-8 machine

;;Opcode Table
;;CHIP-8 has 35 opcodes, which are all two bytes long and stored big-endian. The opcodes are listed below, in hexadecimal and with the following symbols:

;; NNN: address
;; NN: 8-bit constant
;; N: 4-bit constant
;; X and Y: 4-bit register identifier
;; PC : Program Counter
;; I : 16bit register (For memory address) (Similar to void pointer)

(def chip-8-opcode-template-table [
 '("00E0" (fn ))
 '("00EE" (fn ))
 '("0NNN" (fn ))
 '("1NNN" (fn ))
 '("2NNN" (fn ))
 '("3XNN" (fn ))
 '("4XNN" (fn ))
 '("5XY0" (fn ))
 '("6XNN" (fn ))
 '("7XNN" (fn ))
 '("8XY0" (fn ))
 '("8XY1" (fn ))
 '("8XY2" (fn ))
 '("8XY3" (fn ))
 '("8XY5" (fn ))
 '("8XY6" (fn ))
 '("8XY7" (fn ))
 '("8XYE" (fn ))
 '("9XY0" (fn ))
 '("ANNN" (fn ))
 '("BNNN" (fn ))
 '("CXNN" (fn ))
 '("DXYN" (fn ))
 '("EX9E" (fn ))
 '("EXA1" (fn ))
 '("FX07" (fn ))
 '("FX0A" (fn ))
 '("FX15" (fn ))
 '("FX18" (fn ))
 '("FX1E" (fn ))
 '("FX29" (fn ))
 '("FX33" (fn ))
 '("FX55" (fn ))
 '("FX65" (fn ))
 ])

(def hex-digit-regex-str "[0-9a-zA-Z]")

(def opcode-parameters-conversion
  [{:template #"NNN" :capture (str hex-digit-regex-str "{3}")}
   {:template #"NN" :capture (str hex-digit-regex-str "{2}")}
   {:template #"N" :capture (str hex-digit-regex-str "{1}")}
   {:template #"X" :capture (str hex-digit-regex-str "{1}")}
   {:template #"Y" :capture (str hex-digit-regex-str "{1}")}]
)



(defn opcode-template-to-capture
  [template]
;;  (reduce #() template opcode-parameters-conversion)
)



