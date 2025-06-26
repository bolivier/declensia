(ns declensia.core
  (:require [clojure.string :as str]))

(defn regexp? [x]
  (instance? java.util.regex.Pattern x))

(def ^:dynamic *debug*
  "Set this to true to print debug output about the match found for a particular word.

   Note: you probably don't want to enable this. It's for debugging declensia."
  false)

(defn rule
  "Create a replacement rule.

  `target` can be a string or a regexp

  `replacement` can be a string or a function taking the match from `target`. It
  can also reference capture groups with \"$1\" (note: this will error without a
  matching group)"
  [target replacement]
  {:target      target
   :replacement replacement})

(def valid-ruleset?
  #{:uncountable :irregular :regular})

(def empty-rules
  {:uncountable '()
   :irregular   '()
   :regular     '()})

(def ^:dynamic *pluralize-rules*
  "Rules for pluralizing words.

  Prefer modifying this with `add-rule`, it manages precedent."
  empty-rules)

(def ^:dynamic *singularize-rules*
  "Rules for singularizing words.

  Prefer modifying this with `add-rule`, it manages precedent."
  empty-rules)

(defn add-rule
  "Adds a new rule.  Rules are prepended to the existing rulesets,
so if you add general rules, they can override more broad rules.

  `number` is either `:singular` or `:plural`.

  `rule-type` is one of
    - `:uncountable` words that won't be transformed
    - `:irregular` words that have some irregular shape and need precendence over regugular rules.
    - `:regular` normal word rules.  Most of these use regexps and capture many cases.

  `rule` is a rule created with `declensia.core/rule`"
  ([number rule]
   (add-rule number :regular rule))
  ([number rule-type rule]
   (alter-var-root (case number
                     :plural   #'*pluralize-rules*
                     :singular #'*singularize-rules*
                     (throw (ex-info "Invalid ruleset for inflecting"
                                     {:number number
                                      :rule   rule})))
                   (fn [rules]
                     (when-not (valid-ruleset? rule-type)
                       (println
                         rule-type
                         "is not a known ruleset. This rule will not be used."))
                     (update rules rule-type #(cons rule %))))))

;; Add irregular rules
(add-rule :plural :irregular (rule "ox" "oxen"))
(add-rule :singular :irregular (rule "oxen" "ox"))
(add-rule :plural :irregular (rule "octopus" "octopi"))
(add-rule :plural :irregular (rule "person" "people"))
(add-rule :singular :irregular (rule "people" "person"))
(add-rule :plural :irregular (rule "child" "children"))
(add-rule :singular :irregular (rule "children" "child"))
(add-rule :singular :irregular (rule #"(?i)(l|m)ice" "$1ouse"))
(add-rule :plural :irregular (rule #"(?i)(l|m)ouse" "$1ice"))
(add-rule :singular :irregular (rule #"(?i)(phenomen|criteri)a" "$1on"))
(add-rule :plural :irregular (rule #"(?i)(phenomen|criteri)on" "$1a"))
(add-rule :plural
          :irregular
          (rule #"(?i)(alumn|syllab|nucle|fung|cact)us" "$1i"))
(add-rule :singular
          :irregular
          (rule #"(?i)(alumn|syllab|nucle|fung|cact)i" "$1us"))

;; Add uncountable rules
(doseq [w ["air"
           "aircraft"
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

;; Add pluralizing rules
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
    (rule #"(?i)(matr|vert|ind|append)(?:ix|ex)$" "$1ices")
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

;; Add singularizing rules
(doseq
  [r
   [(rule #"s$" "")
    (rule #"(?i)([^f])ves$" "$1fe")
    (rule #"(?i)(hive)s$" "$1")
    (rule #"(?i)(tive)s$" "$1")
    (rule #"thieves" "thief")
    (rule #"(?i)([lr])ves$" "$1f")
    (rule #"(?i)(vert|ind)ices$" "$1ex")
    (rule #"(?i)(matr|append)ices$" "$1ix")
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
  "Return the pluralized noun.

  This multimethod dispatches on the identity of the word.  You can add a one-off replacement like this

  (defmethod pluralize \"foobar\" [_]
    \"foobarium\")"
  identity)

(defn inflect-rule-type
  "This iterates over a single ruleset, either finding a match or returning nil."
  [word ruleset]
  (when-let [rule (first ruleset)]
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
        (recur word
               (rest ruleset))))))

(defmulti singularize
  "Returns the singularized noun.

   This multimethod dispatches on the identity of the word.  You can add a one-off replacement like this

  (defmethod pluralize \"foobaria\" [_]
    \"foobar\")"
  identity)

(defn inflect
  "Mechanism fn for creating singular and plurals.

  This iterates over the rulesets in the rule groups, in order of most to least
  specific. Uncountable words are considered first, then irregular, then
  regular. "
  [word rules]
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
