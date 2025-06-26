(ns declensia.core-test
  (:require [declensia.core :as sut]
            [expectations.clojure.test :as e]))

(def cases
  "Either a word that won't change, or a vector pair [singular, plural]"
  ["equipment"
   "fish"
   "information"
   "jeans"
   "money"
   "police"
   "rice"
   "series"
   "sheep"
   "species"
   ["ability" "abilities"]
   ["address" "addresses"]
   ["agency" "agencies"]
   ["alias" "aliases"]
   ["alias" "aliases"]
   ["amenity" "amenities"]
   ["analysis" "analyses"]
   ["archive" "archives"]
   ["axis" "axes"]
   ["basis" "bases"]
   ["bird" "birds"]
   ["box" "boxes"]
   ["buffalo" "buffaloes"]
   ["bus" "buses"]
   ["case" "cases"]
   ["category" "categories"]
   ["child" "children"]
   ["comment" "comments"]
   ["crisis" "crises"]
   ["database" "databases"]
   ["datum" "data"]
   ["day" "days"]
   ["diagnosis" "diagnoses"]
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
   ["life" "lives"]
   ["louse" "lice"]
   ["man" "men"]
   ["matrix" "matrices"]
   ["medium" "media"]
   ["mouse" "mice"]
   ["movie" "movies"]
   ["newsletter" "newsletters"]
   ["octopus" "octopi"]
   ["ox" "oxen"]
   ["parenthesis" "parentheses"]
   ["person" "people"]
   ["perspective" "perspectives"]
   ["photo" "photos"]
   ["portfolio" "portfolios"]
   ["prize" "prizes"]
   ["process" "processes"]
   ["query" "queries"]
   ["quiz" "quizzes"]
   ["search" "searches"]
   ["sex" "sexes"]
   ["shoe" "shoes"]
   ["stack" "stacks"]
   ["status" "statuses"]
   ["switch" "switches"]
   ["testis" "testes"]
   ["thief" "thieves"]
   ["tomato" "tomatoes"]
   ["vertex" "vertices"]
   ["virus" "viruses"]
   ["weather" "weather"]
   ["wife" "wives"]
   ["wish" "wishes"]
   ["woman" "women"]
   ["zombie" "zombies"]
   ['ability 'abilities]
   [:ability :abilities]])

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

(comment
  (binding [sut/*debug* true]
    (let [word "buses"]
      (sut/singularize word))))
