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

(add-rule :plural :irregular (rule ::octo "octopus" "octopi"))

(doseq [r [(rule ::base #"$" "s")
           (rule ::xes #"(?i)(ax|test)is" "$1es")
           (rule ::us #"(?i)(.*)us" "$1uses")
           (rule ::octo #"(?i)(octop)us" "$1i")]]
  (add-rule :plural r))

(doseq [r [(rule ::base #"s$" "")
           (rule ::xes #"^(a)x[ie]s$" "$1xis")
           (rule ::octo #"(?i)(.*)(us|i|uses)$" "$1us")]]
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
          (str/replace word
                       target
                       replacement)
          (recur (rest rules)))))))



(defn inflect [word rules]
  (loop [rulesets (map rules [:uncountable :irregular :regular])]
    (if (empty? rulesets)
      word
      (if-let [match (inflect-rule-type word
                                        (first rulesets))]
        match
        (recur (rest rulesets))))))

(defmethod pluralize :default
  [word]
  (inflect word *pluralize-rules*))

(defmulti singularize
  "Returns the singularized noun."
  :none)

(defmethod singularize :default
  [word]
  (inflect word *singularize-rules*))
