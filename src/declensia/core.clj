(ns declensia.core
  (:require [clojure.string :as str]))

(defn regexp? [x]
  (instance? java.util.regex.Pattern x))

(defn rule [id target replacement]
  {:id          id
   :type        :rule
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

(add-rule :plural :irregular (rule ::octo "ox" "oxen"))
(add-rule :singular :irregular (rule ::octo "oxen" "ox"))
(add-rule :plural :irregular (rule ::octo "octopus" "octopi"))
(add-rule :plural :irregular (rule ::person "person" "people"))
(add-rule :singular :irregular (rule ::person "people" "person"))
(add-rule :plural :irregular (rule ::children "child" "children"))
(add-rule :singular :irregular (rule ::children "children" "child"))
(add-rule :singular :irregular (rule ::mice #"(?i)(l|m)ice" "$1ouse"))
(add-rule :plural :irregular (rule ::mice #"(?i)(l|m)ouse" "$1ice"))

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
  (add-rule :singular :uncountable (rule (keyword w) w w))
  (add-rule :plural :uncountable (rule (keyword w) w w)))

(doseq
  [r
   [(rule ::base #"$" "s")
    (rule
      ::paren
      #"(?i)((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$"
      "$1ses")
    (rule ::man #"(wom|m)an$" "$1en")
    (rule ::life #"(.*)fe?$" "$1ves")
    (rule ::xes #"(?i)(ax|test)is$" "$1es")
    (rule ::es #"(?i)(x|ch|ss|sh)$" "$1es")
    (rule ::ices #"(?i)(matr|vert|ind)(?:ix|ex)$" "$1ices")
    (rule ::buffalo #"(?i)(buffal|tomat)o$" "$1oes")
    (rule ::as #"(?i)(.*)as$" "$1ases")
    (rule ::media #"(?i)([ti])um$" "$1a")
    (rule ::vowels #"(?i)([^aeiouy]|qu)y$" "$1ies")
    (rule ::query #"(?i)(quer)y$" "$1ies")
    (rule ::us #"(?i)(.*)us$" "$1uses")
    (rule ::octo #"(?i)(octop)us$" "$1i")
    (rule ::crisis #"(?i)(cris|test)(is|es)$" "$1es")
    (rule ::database #"(?i)(database)s" "$1")
    (rule ::quiz #"(?i)(qui)z$" "$1zzes")]]
  (add-rule :plural r))

(doseq
  [r
   [(rule ::base #"s$" "")
    (rule ::life #"(?i)([^f])ves$" "$1fe")
    (rule ::hive #"(?i)(hive)s$" "$1")
    (rule ::persp #"(?i)(tive)s$" "$1")
    (rule ::thief #"thieves" "thief")
    (rule ::halves #"(?i)([lr])ves$" "$1f")
    (rule ::vertex #"(?i)(vert|ind)ices$" "$1ex")
    (rule ::matrix #"(?i)(matr)ices$" "$1ix")
    (rule nil #"(?i)(s)eries$" "$1eries")
    (rule ::vowels #"(?i)([^aeiouy]|qu)ies$" "$1y")
    (rule ::bus #"(?i)(bus)es" "$1")
    (rule ::man #"(wom|m)en$" "$1an")
    (rule ::media #"(?i)([ti])a$" "$1um")
    (rule ::alias #"(?i)(alias)es$" "$1")
    (rule ::buffalo #"(?i)(o)es$" "$1")
    (rule ::series #"(?i)(quer)ies$" "$1y")
    (rule ::shoes #"shoes" "shoe")
    (rule :es #"(?i)(x|ch|ss|sh)es$" "$1")
    (rule ::xes #"^(a)x[ie]s$" "$1xis")
    (rule nil #"(?i)(m)ovies$" "$1ovie")
    (rule ::octo #"(?i)(octop|stat|vir)(us|i|uses)$" "$1us")
    (rule ::crisis #"(?i)(cris|test)(is|es)$" "$1is")
    (rule ::zombie "zombies" "zombie")
    (rule
      ::paren
      #"(?i)((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$"
      "$1sis")
    (rule ::database #"(?i)(database)s" "$1")
    (rule ::quiz #"(?i)(quiz)zes$" "$1")]]
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
