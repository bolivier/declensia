(ns declensia.core-test
  (:require [declensia.core :as sut]
            [expectations.clojure.test :as e])
  (:import java.io.StringWriter))

(def cases
  "Either a word that won't change, or a vector pair [singular, plural]"
  ["aircraft"
   "equipment"
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
   [:ability :abilities]
   ;; extra unusual words
   ["cactus" "cacti"]
   ["fungus" "fungi"]
   ["nucleus" "nuclei"]
   ["syllabus" "syllabi"]
   ["alumnus" "alumni"]
   ["criterion" "criteria"]
   ["phenomenon" "phenomena"]
   ["appendix" "appendices"]
   ["index" "indices"]
   ["matrix" "matrices"]
   ["thesis" "theses"]])

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

(e/defexpect error-cases
  (binding [*out* (StringWriter.)]
    (sut/add-rule :singular :bad-type (sut/rule "hello" "hello"))
    (e/expect ":bad-type is not a known ruleset. This rule will not be used.\n"
              (str *out*)))

  (try
    (sut/add-rule :bad :uncountable (sut/rule "hello" "hello"))
    (catch Exception e
      (e/expect "Invalid ruleset for inflecting" (.getMessage e)))))

(comment
  (binding [sut/*debug* true]
    (let [word "buses"]
      (sut/singularize word))))
