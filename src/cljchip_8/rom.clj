 (ns cljchip-8.rom
   (:require [cljchip-8.proj-asserts :refer :all]
             [clojure.java.io :refer [input-stream file]]
             [cljchip-8.binary :refer :all]))

(defn filepath->file
  [filepath]
  (file filepath))

(defn file->java-byte-array
  "Provide an object of the file type supplied by clojure.java.io, and an integer size of the byte-array"
  [file]
  (assert-is-file file)
  (with-open [in (input-stream file)]
    (let [buf (byte-array (.length file))]
      (.read in buf)
      buf)))

(defn java-byte-array->bytes
  [java-byte-array]
  (->> java-byte-array
      (vec)
      (map value->byte)))

(defn rom-filepath->rom
  [rom-filepath]
  (-> rom-filepath
      filepath->file
      file->java-byte-array
      java-byte-array->bytes))
