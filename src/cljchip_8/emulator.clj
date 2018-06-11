(ns cljchip-8.emulator)

;;initialze memory at the begining of an emulation for program contents
;;given a rom, return a chip-8 with populated memory
(defn chip-8-initialize
  [rom])

(defn chip-8-emulate
  [rom]
  (let [chip-8 (chip-8-initialize rom)]
    ;;Rom in memory
    ;;loop execution where the result machine of one instruction feeds into the next
  rom))
