(ns lmtalk.data
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def *corpus-dir* "/home/wqiu/nlg/wqiu/thesis/corpus/TACoS/alignments-text")

(defn load-data 
  "load the video-descprition alignment data."
  [input-dir]
  (into {} (for [f (file-seq (io/file input-dir))
        :let [myname (.getName f)
              path (.getCanonicalPath f)]
        :when (.endsWith path ".tsv")]
    [(first (str/split myname #"\.")) (read-content f)])))

(defn read-content 
  "read the content of file in corpus."
  [file]
  (let [rdr (io/reader file)]
    (for [line (line-seq rdr)]
      (parse-line line))))

(defn parse-line 
  "parse one line according to the corpus data format"
  [line]
  (let [paddedline (str line "_")
        content (vec (butlast (str/split paddedline #"\t")))]
    {:time [(get content 0) (get content 1)]
     :action (get content 2)
     :tool (let [tool (get content 3)] (if (str/blank? tool) nil (str/split tool #",")))
     :patient (let [patient (get content 4)] (if (str/blank? patient) nil (str/split patient #",")))
     :location (let [location (get content 5)] (if (str/blank? location) nil (str/split location #",")))
     :nls (filter #(not (str/blank? %)) (subvec content 6))
     }))

(take 2 (load-data *corpus-dir*))

(with-open [wtr (io/writer "tacos.txt")]
  (pprint (load-data *corpus-dir* ) wtr))
;(concat (repeat 3 :start) '("i" "love")  :end)
;


