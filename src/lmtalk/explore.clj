(def lm (lang-model (build-lm *samplecorpus* 2)))

(lm '("girls") '("takes"))

((verbalize-bin raw-model 10)  "takes" "orange") 

((verbalize (verbalize-bin raw-model 10)) "person" "takes" "orange")
