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
   ["status" "statuses"]
   ["man" "men"]
   ["woman" "women"]
   ["person" "people"]
   ["sex" "sexes"]
   ["child" "children"]
   ["mouse" "mice"]
   ["shoe" "shoes"]
   ["crisis" "crises"]
   ["alias" "aliases"]
   ["zombie" "zombies"]
   ["parenthesis" "parentheses"]
   ["analysis" "analyses"]
   "equipment"
   "information"
   "rice"
   "money"
   "species"
   "series"
   "fish"
   "sheep"
   "jeans"
   "police"
   ["ox" "oxen"]
   ["life" "lives"]
   ["thief" "thieves"]
   ["quiz" "quizzes"]
   ["ability" "abilities"]
   [:ability :abilities]
   ['ability 'abilities]
   ["address" "addresses"]
   ["address" "address"]
   ["agency" "agencies"]
   ["alias" "aliases"]
   ["alias" "alias"]
   ["amenity" "amenities"]
   ["analysis" "analyses"]
   ["analysis" "analysis"]
   ["archive" "archives"]
   ["axis" "axes"]
   ["basis" "bases"]
   ["box" "boxes"]
   ["buffalo" "buffaloes"]
   ["bus" "buses"]
   ["bus" "bus"]
   ["case" "cases"]
   ["category" "categories"]
   ["comment" "comments"]
   ["crisis" "crises"]
   ["crisis" "crisis"]
   ["database" "databases"]
   ["datum" "data"]
   ["day" "days"]
   ["diagnosis" "diagnoses"]
   ["diagnosis" "diagnosis"]
   ["dwarf" "dwarves"]
   ["edge" "edges"]
   ["elf" "elves"]
   ["experience" "experiences"]
   ["fix" "fixes"]
   ["foobar" "foobars"]
   ["half" "halves"]
   ["horse" "horses"]
   ["house" "houses"]
   ["index" "indices"]
   ["louse" "lice"]
   ["matrix" "matrices"]
   ["medium" "media"]
   ["mouse" "mice"]
   ["movie" "movies"]
   ["newsletter" "newsletters"]
   ["octopus" "octopi"]
   ["ox" "oxen"]
   ["perspective" "perspectives"]
   ["photo" "photos"]
   ["portfolio" "portfolios"]
   ["prize" "prizes"]
   ["process" "processes"]
   ["query" "queries"]
   ["quiz" "quizzes"]
   ["safe" "saves"]
   ["search" "searches"]
   ["shoe" "shoes"]
   ["stack" "stacks"]
   ["status" "statuses"]
   ["switch" "switches"]
   ["testis" "testes"]
   ["tomato" "tomatoes"]
   ["vertex" "vertices"]
   ["wife" "wives"]
   ["wish" "wishes"]
   ["weather" "weather"]])


(e/defexpect plural-singular-test
  (doseq [case cases]
    (let [[singular plural] (if (vector? case)
                              case
                              [case case])]
      (e/expect plural
                (sut/pluralize singular)
                (str "Failed to pluralize " singular))
      (e/expect singular
                (sut/singularize plural)
                (str "Failed to singularize " plural)))))

(let [word "octopus"]
  [(sut/singularize (sut/pluralize word)) (sut/pluralize word)])
