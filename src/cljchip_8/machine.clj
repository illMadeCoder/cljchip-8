(ns cljchip-8.machine
  (:require [cljchip-8.binary :refer :all]))

;;memory width is the width of a single memory location
(def memory-width 16)
;;memory-size is the total amount of memory locations in the machine
(def memory-size 4096)
;;memory is simply a vector of count memory-size and where each member is a binary of memory-width
(def memory-prototype (vec (doall (repeat memory-size (value->binary 0 memory-width)))))

;;Goes down to 0 at 60 hertz each
(def delay-timer-prototype 0)
;;If above 0 play beep noise
(def sound-timer-prototype 0)

;;Registers V0 .. VF : 8 bit (2 nibs), I 16 bit (4 nibs)
;;Used to create new machines per emulation
(def chip-8-prototype {:memory memory-prototype
                       :V0 (value->binary 0 8) 
                       :V1 (value->binary 0 8)
                       :V2 (value->binary 0 8)
                       :V3 (value->binary 0 8)
                       :V4 (value->binary 0 8)
                       :V5 (value->binary 0 8)
                       :V6 (value->binary 0 8)
                       :V7 (value->binary 0 8)
                       :V8 (value->binary 0 8)
                       :V9 (value->binary 0 8)
                       :VA (value->binary 0 8)
                       :VB (value->binary 0 8)
                       :VC (value->binary 0 8)
                       :VD (value->binary 0 8)
                       :VE (value->binary 0 8)
                       :VF (value->binary 0 8)
                       :I  (value->binary 0 16)
                       :PC (value->binary 0 12)
                       :delay-timer delay-timer-prototype
                       :sound-timer sound-timer-prototype})
