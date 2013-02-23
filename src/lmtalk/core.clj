(ns lmtalk.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def *corpus-dir* "/media/Program_Data/Research/thesis/corpus/TACoS/alignments-text")

(defn load-data [input-dir]
  "load the video-descprition alignment data."
  (into {} (for [f (file-seq (io/file input-dir))
        :let [myname (.getName f)
              path (.getCanonicalPath f)]
        :when (.endsWith path ".tsv")]
    [(first (str/split myname #"\.")) (read-content f)])))

(defn read-content [file]
  "read the content of file in corpus."
  (let [rdr (io/reader file)]
    (for [line (line-seq rdr)]
      (parse-line line))))

(defn parse-line [line]
  "parse one line according to the corpus data format"
  (let [paddedline (str line "_")
        content (vec (butlast (str/split paddedline #"\t")))]
    {:time [(get content 0) (get content 1)]
     :action (get content 2)
     :tool (let [tool (get content 3)] (if (empty? tool) nil (str/split tool #",")))
     :patient (let [patient (get content 4)] (if (empty? patient) nil (str/split patient #",")))
     :location (let [location (get content 5)] (if (empty? location) nil (str/split location#",")))
     :nls (mapv #(if (empty? %) nil %) (subvec content 6))
     }))

(pprint (first (load-data *corpus-dir*)))

;(concat (repeat 3 :start) '("i" "love")  :end)
;


