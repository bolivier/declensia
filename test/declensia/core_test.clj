(ns declensia.core-test
  (:require [declensia.core :as sut]
            [expectations.clojure.test :as e]))



(def cases
  [["bird" "birds"]
   ["axis" "axes"]
   #_["AXIS" "AXES"]
   ["octopus" "octopi"]
   ["virus" "viruses"]
   ["bus" "buses"]
   ["status" "statuses"]])

(e/defexpect plural-singular-test
  (doseq [[singular plural] cases]
    (e/expect plural
              (sut/pluralize singular)
              (str "Failed to pluralize " singular))
    (e/expect singular
              (sut/singularize plural)
              (str "Failed to singularize " plural))))

(let [word "axis"]
  [(sut/singularize (sut/pluralize word)) (sut/pluralize word)])
