(ns cljchip-8.programmability
  (:require [cljchip-8.binary :refer :all]))

;;* Programmability
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


(defn Vx
  [nibble]
  (keyword (str "V" (binary->hexstring nibble))))

(def opcode-width 16)
(def machine-memory-width 8)

;;chip-8 interface
;;:memory :: 4096, 16 bit binaries
;;:v0-vF :: each are 8 bit binaries
;;:I :: 16 bit binary
;;:PC :: 12 bit binary
;;:delay-timer :: whole number
;;:sound-timer :: whole number

(defn PC-inc
  [chip-8]
  (assoc chip-8 :PC (bin-add :PC chip-8 (/ opcode-width machine-memory-width))))

(def opcode-template-table
  [{ :template "00E0"
    :behavior (fn [chip-8]
                ;;Clears the screen
                ;;disp_clear()
                
                )}
   { :template "00EE"
    :behavior (fn [chip-8]
                ;;Returns from a subroutine
                ;;return:
                )}
   { :template "0NNN"
    :behavior (fn [chip-8 NNN ]
                ;;Calls 1802 program at address NNN.
                ;;
                )}
   { :template "1NNN"
    :behavior (fn [chip-8 NNN]
                ;;Jumps to address NNN
                ;;goto NNN
                (assoc chip-8 :PC NNN))}
   { :template "2NNN" 
    :behavior (fn [chip-8 NNN]
                ;;Calls subroutine at NNN
                ;;*(0xNNN)()
                
                )}
   { :template "3XNN" 
    :behavior (fn [chip-8 X NN]
                ;;Skips the next instruction if VX equals NN
                ;;if(Vx==NN)
                (if (= ((Vx X) chip-8) NN)
                  (PC-inc chip-8)
                  chip-8))}
   { :template "4XNN" 
    :behavior (fn [chip-8 X NN]
                ;;Skips the next instruction if Vx doesn't equal NN
                ;;if(Vx!=NN)
                (if (not= ((Vx X) chip-8) NN)
                  (PC-inc chip-8)
                  chip-8))}
   { :template "5XY0" 
    :behavior (fn [chip-8 X Y]
                ;;Skips the next instruction if VX equals VY.
                ;;if(Vx==Vy)
                (if (= ((Vx X) chip-8) (Vx Y) chip-8)
                  (PC-inc chip-8)
                  chip-8))}
   { :template "6XNN" 
    :behavior (fn [chip-8 X NN]
                ;;Sets Vx to NN
                ;;Vx=NN
                (assoc chip-8 (Vx X) NN))}
   { :template "7XNN" 
    :behavior (fn [chip-8 X NN]
                ;;Adds NN to Vx (carry flag is not changed)
                ;;Vx += NN
                (assoc chip-8 (Vx X) (bin-add Vx (binary->value NN))))}
   { :template "8XY0" 
    :behavior (fn [chip-8 X Y]
                ;;Sets Vx to the value of Vy
                ;;Vx=Vy
                (assoc chip-8 (Vx X) Y))}
   { :template "8XY1" 
    :behavior (fn [chip-8 X Y]
                ;;Sets Vx to Vx or Vy (bitwise or)
                ;;Vx = Vx or Vy
                (let [resultant (bin-or ((Vx X) chip-8) ((Vx Y) chip-8))]
                  (assoc chip-8 (Vx X) resultant)))}
   { :template "8XY2" 
    :behavior (fn [chip-8 X Y]
                ;;Sets Vx to Vx and Vy (bitwise and)
                ;;Vx = Vx and Vy
                (let [resultant (bin-and ((Vx X) chip-8) ((Vx Y) chip-8))]
                  (assoc chip-8 (Vx X) resultant)))}
   { :template "8XY3" 
    :behavior (fn [chip-8 X Y]
                ;;Sets Vx to Vx xor Vy
                ;;Vx=Vx xor Vy
                (let [resultant (bin-xor ((Vx X) chip-8) ((Vx Y) chip-8))]
                  (assoc chip-8 (Vx X) resultant))
                )}
   { :template "8XY5" 
    :behavior (fn [chip-8]
                )}
   { :template "8XY6" 
    :behavior (fn [chip-8] 
                )}
   { :template "8XY7" 
    :behavior (fn [chip-8] 
                )}
   { :template "8XYE" 
    :behavior (fn [chip-8] 
                )}
   { :template "9XY0" 
    :behavior (fn [chip-8] 
                )}
   { :template "ANNN" 
    :behavior (fn [chip-8] 
                )}
   { :template "BNNN" 
    :behavior (fn [chip-8] 
                )}
   { :template "CXNN" 
    :behavior (fn [chip-8] 
                )}
   { :template "DXYN" 
    :behavior (fn [chip-8] 
                )}
   { :template "EX9E" 
    :behavior (fn [chip-8] 
                )}
   { :template "EXA1" 
    :behavior (fn [chip-8] 
                )}
   { :template "FX07" 
    :behavior (fn [chip-8] 
                )}
   { :template "FX0A" 
    :behavior (fn [chip-8] 
                )}
   { :template "FX15" 
    :behavior (fn [chip-8] 
                )}
   { :template "FX18" 
    :behavior (fn [chip-8] 
                )}
   { :template "FX1E" 
    :behavior (fn [chip-8] 
                )}
   { :template "FX29" 
    :behavior (fn [chip-8] 
                )}
   { :template "FX33" 
    :behavior (fn [chip-8] 
                )}
   { :template "FX55" 
    :behavior (fn [chip-8] 
                )}
   { :template "FX65" 
    :behavior (fn [chip-8] 
                )}])
;;Design
;;Input to module :: opcode, machine
;;Where opcode is a 16 bit wide binary
;;Transform the opcode to an instruction by doing the follow ::
;;1. convert opcode to its hexidecimal string representation hex-opcode
;;  1. (binary->hexstring opcode) :: hex-opcode
;;2. run regex match over the opcode-template tables regex form, where on the first match run the matched regex's associated behavior
;;  1. to get ther regex form of template opcode --
;;    1. all characters that don't exist in template-parameters are literals, and otherwise exchange the parameter for its associaed regex-form.

;;opcode defined as a binary word, or a binary of width 16, 2 bytes, 4 nibbles
(def template-parameters
  [{ :parameter #"NNN" :regex-form "(...)"}
   { :parameter #"NN" :regex-form "(..)"}
   { :parameter #"N" :regex-form "(.)"}
   { :parameter #"X" :regex-form "(.)"}
   { :parameter #"Y" :regex-form "(.)"}])

;;used to convert the opcode template table to a regex table for the purpose of capturing opcodes
(defn- template->regex
  [template]
  ;;create a regex who is capable of matching against an opcode and returning 
  (re-pattern (reduce (fn
                        [regex-cons {parameter :parameter regex-form :regex-form}]
                        (clojure.string/replace regex-cons parameter regex-form))
                      template template-parameters)))

;;execution table is of the opcode-template-table but with the inclusion of a regex, capture used opcode-stringified 
(def opcode-execution-table
  (map (fn [{template :template behavior :behavior :as entry}]
            (dissoc (assoc entry :capture (template->regex template)) :template)) opcode-template-table))
;;Looks like
;{:behavior :: function, :capture :: regex}

;;Takes an opcode and machine, returns the machine with a state change based on the opcode's execution

(defn opcode->capture-able
  [opcode]
  (clojure.string/upper-case (binary->hexstring opcode)))

(defn capture-able->behavior+args
  [capture-able]
  (loop [i 0
         sentinal (count opcode-execution-table)]
    (let [candidate (re-find (:capture (nth opcode-execution-table i)) capture-able)]
      (if (not (nil? candidate))
        (hash-map :behavior (:behavior (nth opcode-execution-table i)) :args (next candidate))
        (if (< i sentinal)
          (recur (inc i) sentinal)
          nil)))))

;;transform an opcode into its hex stringified version
;;find a match on the opcode-execution-table, and extract parameters back into binary
;;run behavior with parameter on machine and return its result
(defn ex
  [behavior+opcode machine]
  (apply (:behavior behavior+opcode) machine (:args behavior+opcode)))

(defn opcode+machine->machine
  [opcode machine]
  (ex (-> opcode
      opcode->capture-able
      capture-able->behavior+args) machine))
