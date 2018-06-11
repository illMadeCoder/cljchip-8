(ns cljchip-8.core
  (:require [cljchip-8.rom :refer :all])
  (:gen-class))

;;* Program Entry Point
(defn -main
  "Accept's ROM file path to execute chip-8 emulator with a program."
  [& args]
  (let [rom-file-path (first args) options (rest args)]
    (-> rom-file-path
        rom-filepath->rom)))
