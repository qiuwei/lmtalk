(ns lmtalk.lm
  (:require [clojure.string :as str]
            [lmtalk.data :as lmdata]))

(defn- get-sent-tacos 
  "extract sentences from tacos"
  [tacos]
  (apply concat (doall (for [video (vals tacos)
        frame video]
    (:nls frame))))
  )

(get-sent-tacos 
  (lmdata/load-data "/home/wqiu/nlg/wqiu/thesis/corpus/TACoS/alignments-text")) 

(def ^:dynamic *samplecorpus* (get-sent-tacos (lmdata/load-data "../corpus/TACoS/alignments-text"))) 

(defn n-gram 
  "generate n gram from a sentence
  sent is a sequence of words"
  [sent n]
  (let [spad (repeat (if (= 1 n) n (dec n)) :start)
        epad '(:end)]
    (partition n 1 (concat spad sent epad))))

(defn build-lm 
  "build raw n-gram language model from corpus.
  corpus is a sequence of strings.
  Each string represents a sentence."
  [corpus n]
  (let [normalize (fn [s] (.toLowerCase s))]
    (apply merge
      (for [x (range 1 (inc n))] 
        (frequencies (apply concat (doall (pmap #(n-gram (str/split (normalize %) #"\.|\s+") x) corpus))))))))

(defn lang-model
  "get the conditional probability of current word and history.
  raw language model is a map representation of ngrams.
  return a fucntion which accepts history and current word to calculate the
  probability. 
  TODO: implement smoothing method as a function which can be passed to it"
  [langmodel]
  (memoize (fn [history current]
    (let [count-his-current (langmodel (concat history current))
        count-his (langmodel history)]
      (if (and count-his-current count-his) 
        (/ count-his-current count-his)
        0)))))


(defn- get-next-state
  "get next state according to current state and vocab and lanuage model"
  [lm cstat vocab]
  (doall (pmap 
    (fn [word] 
      (reduce  #(if (> (second %) (second %2)) % %2)
              (doall (pmap #(vector (concat word (first %)) (* (second %) (lm (-> % ffirst list) word))) cstat))))
    vocab)))

(defn verbalize-bin 
  "get the most probable utterance according to the start word and end word
  This is only for 2-gram language model"
  [langmodel len]
  (fn [start end] 
    (let [vocab (filter #(and (= 1 (count %)) (not (keyword? (first %)))) (keys langmodel))
          lm (lang-model langmodel)
          init (repeat (count vocab) [(list start) 1])
          gns (partial get-next-state lm)]
      ;this can generate a lazy sequence of states
      #_(iterate #(gns % vocab) init)
      #_(take len (for [stat (iterate #(gns % vocab) init)]
            (gns stat (-> end list list))))
      (reduce (fn [x y] 
                 (let [[[x1 x2]] x
                       [[y1 y2]] y]
                   (if (< (/ x2 (count x1)) (/ y2 (count y1))) y x)))
             (take len (for [stat (iterate #(gns % vocab) init)]
            (gns stat (-> end list list))))))))

(defn verbalize 
  "return a fuction which is used to get the most probable utterance according to the config.
  The function accepts config which is a list specifies the order of content words.
  fn-bin is the function deal with binary config which specifies the start word and end word"
  [fn-bin]
  (fn [& config]
    (let [conf (partition 2 1 (concat [:start] config [:end]))
          result-bin (doall (pmap #(apply fn-bin %) conf))]
       (reduce #(concat % (reverse %2)) '() (doall (for [[[ x _]] result-bin]
        (butlast x)))))))
