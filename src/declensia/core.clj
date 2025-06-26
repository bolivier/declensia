(ns declensia.core
  (:require [clojure.string :as str]))

(defn regexp? [x]
  (instance? java.util.regex.Pattern x))

(defn rule [target replacement]
  {:type        :rule
   :target      target
   :replacement replacement})

(def empty-rules
  {:uncountable '()
   :irregular   '()
   :regular     '()})

(def ^:dynamic *pluralize-rules*
  empty-rules)

(def ^:dynamic *singularize-rules*
  empty-rules)

(defn add-rule
  ([number rule]
   (add-rule number :regular rule))
  ([number rule-type rule]
   (alter-var-root (case number
                     :plural   #'*pluralize-rules*
                     :singular #'*singularize-rules*
                     (throw (ex-info "Invalid ruleset for inflecting"
                                     {:number number
                                      :rule   rule})))
                   (fn [rules] (update rules rule-type #(cons rule %))))))

(add-rule :plural :irregular (rule "ox" "oxen"))
(add-rule :singular :irregular (rule "oxen" "ox"))
(add-rule :plural :irregular (rule "octopus" "octopi"))
(add-rule :plural :irregular (rule "person" "people"))
(add-rule :singular :irregular (rule "people" "person"))
(add-rule :plural :irregular (rule "child" "children"))
(add-rule :singular :irregular (rule "children" "child"))
(add-rule :singular :irregular (rule #"(?i)(l|m)ice" "$1ouse"))
(add-rule :plural :irregular (rule #"(?i)(l|m)ouse" "$1ice"))

(defonce ^:dynamic *debug*
  false)

(doseq [w ["air"
           "alcohol"
           "art"
           "blood"
           "butter"
           "cheese"
           "chewing"
           "coffee"
           "confusion"
           "cotton"
           "education"
           "electricity"
           "entertainment"
           "equipment"
           "fiction"
           "fish"
           "food"
           "forgiveness"
           "fresh"
           "gold"
           "gossip"
           "grass"
           "ground"
           "gum"
           "happiness"
           "history"
           "homework"
           "honey"
           "ice"
           "information"
           "jam"
           "jeans"
           "knowledge"
           "lightning"
           "liquid"
           "literature"
           "love"
           "luck"
           "luggage"
           "meat"
           "milk"
           "mist"
           "money"
           "music"
           "news"
           "oil"
           "oxygen"
           "paper"
           "patience"
           "peanut"
           "pepper"
           "petrol"
           "police"
           "pork"
           "power"
           "pressure"
           "research"
           "rice"
           "sadness"
           "series"
           "sheep"
           "shopping"
           "silver"
           "snow"
           "space"
           "species"
           "speed"
           "steam"
           "sugar"
           "sunshine"
           "tea"
           "tennis"
           "thunder"
           "time"
           "toothpaste"
           "traffic"
           "up"
           "vinegar"
           "washing"
           "weather"
           "wine"
           "wood"
           "wool"]]
  (add-rule :singular :uncountable (rule w w))
  (add-rule :plural :uncountable (rule w w)))

(doseq
  [r
   [(rule #"$" "s")
    (rule
      #"(?i)((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$"
      "$1ses")
    (rule #"(wom|m)an$" "$1en")
    (rule #"(.*)fe?$" "$1ves")
    (rule #"(?i)(ax|test)is$" "$1es")
    (rule #"(?i)(x|ch|ss|sh)$" "$1es")
    (rule #"(?i)(matr|vert|ind)(?:ix|ex)$" "$1ices")
    (rule #"(?i)(buffal|tomat)o$" "$1oes")
    (rule #"(?i)(.*)as$" "$1ases")
    (rule #"(?i)([ti])um$" "$1a")
    (rule #"(?i)([^aeiouy]|qu)y$" "$1ies")
    (rule #"(?i)(quer)y$" "$1ies")
    (rule #"(?i)(.*)us$" "$1uses")
    (rule #"(?i)(octop)us$" "$1i")
    (rule #"(?i)(cris|test)(is|es)$" "$1es")
    (rule #"(?i)(database)s" "$1")
    (rule #"(?i)(qui)z$" "$1zzes")]]
  (add-rule :plural r))

(doseq
  [r
   [(rule #"s$" "")
    (rule #"(?i)([^f])ves$" "$1fe")
    (rule #"(?i)(hive)s$" "$1")
    (rule #"(?i)(tive)s$" "$1")
    (rule #"thieves" "thief")
    (rule #"(?i)([lr])ves$" "$1f")
    (rule #"(?i)(vert|ind)ices$" "$1ex")
    (rule #"(?i)(matr)ices$" "$1ix")
    (rule #"(?i)(s)eries$" "$1eries")
    (rule #"(?i)([^aeiouy]|qu)ies$" "$1y")
    (rule #"(?i)(bus)es" "$1")
    (rule #"(wom|m)en$" "$1an")
    (rule #"(?i)([ti])a$" "$1um")
    (rule #"(?i)(alias)es$" "$1")
    (rule #"(?i)(o)es$" "$1")
    (rule #"(?i)(quer)ies$" "$1y")
    (rule #"shoes" "shoe")
    (rule #"(?i)(x|ch|ss|sh)es$" "$1")
    (rule #"^(a)x[ie]s$" "$1xis")
    (rule #"(?i)(m)ovies$" "$1ovie")
    (rule #"(?i)(octop|stat|vir)(us|i|uses)$" "$1us")
    (rule #"(?i)(cris|test)(is|es)$" "$1is")
    (rule "zombies" "zombie")
    (rule
      #"(?i)((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$"
      "$1sis")
    (rule #"(?i)(database)s" "$1")
    (rule #"(?i)(quiz)zes$" "$1")]]
  (add-rule :singular r))

(defmulti pluralize
  "Return the pluralized noun."
  identity)

(defn inflect-rule-type [word rules]
  (loop [rules rules]
    (when-let [rule (first rules)]
      (let [{:keys [target replacement]} rule]
        (if (or (= target word)
                (and (regexp? target)
                     (re-find target
                              word)))
          (do (when *debug*
                    (println "using rule")
                    (prn rule))
              (str/replace word
                           target
                           replacement))
          (recur (rest rules)))))))

(defmulti singularize
  "Returns the singularized noun."
  identity)

(defn inflect [word rules]
  (let [serializer (cond
                     (string? word)  identity
                     (keyword? word) keyword
                     (symbol? word)  symbol
                     :else           identity)]
    (loop [rulesets (map rules [:uncountable :irregular :regular])]
      (if (empty? rulesets)
        word
        (if-let [match (inflect-rule-type (name word)
                                          (first rulesets))]
          (serializer match)
          (recur (rest rulesets)))))))

(defmethod pluralize :default
  [word]
  (inflect word *pluralize-rules*))

(defmethod singularize :default
  [word]
  (inflect word *singularize-rules*))
