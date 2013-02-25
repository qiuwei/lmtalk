(def raw-model (build-lm *samplecorpus* 2))

(def lm (lang-model raw-model))

(time (lm '("girl") '("takes")))

(time (lm '("girl") '("takes")))

(time ((verbalize-bin raw-model 10)  "takes" "orange")) 

((verbalize-bin raw-model 10)  "takes" "knife") 


(def talk (verbalize (verbalize-bin raw-model 3)))

(talk "takes" "knife")

