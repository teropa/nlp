(ns teropa.nlp.tokenizer.punkt-token
  "Stores a token of text with annotations produced during 
   sentence boundary detection"
  (:use teropa.nlp.util)
  (:import [java.util.regex Pattern]))

(def re-ellipsis  #"\.\.+$")
(def re-numeric   #"^-?[\.,]?\d[\d,\.-]*\.?$")
(def re-initial   #"[^\W\d]\.$")
(def re-alpha     #"[^\W\d]+$")
(def re-non-punct #"[^\W\d]")

(defprotocol PunktTokenAttributes
  (get-type [this] "Returns a case-normalized representation of the token")
  (type-no-period [this] "The type with its final period removed if it has one")
  (type-no-sentperiod [this] "The type with its final period removed if it is marked as a sentence break")
  (first-upper? [this] "True if the token's first character is uppercase")
  (first-lower? [this] "True if the token's first character is lowercase")
  (first-case [this])
  (is-ellipsis? [this] "True if the token's text is that of an ellipsis")
  (is-number? [this] "True if the token's text is that of a number")
  (is-initial? [this] "True if the token's text is that of an initial")
  (is-alpha? [this] "True if the token's text is all alphabetic")
  (is-non-punct? [this] "True if the token is either a number or is alphabetic"))
  
(defrecord PunktToken [tok period-final parastart linestart sentbreak abbr]
  PunktTokenAttributes
  (get-type
    [this]
    (-> re-numeric
        (.matcher (lower tok))
        (.replaceAll "##number##")))
  (type-no-period [this]
    (let [type (get-type this)]
      (if (.endsWith type ".")
        (substr type 0 -1)
        type)))
  (type-no-sentperiod [this]
    (if (:sentbreak this)
      (type-no-period this)
      (get-type this)))
  (first-upper? [this]
    (Character/isUpperCase (.charAt tok 0)))
  (first-lower? [this]
    (Character/isLowerCase (.charAt tok 0)))
  (first-case [this]
    (cond
      (first-lower? this) :lower
      (first-upper? this) :upper
      :else :none))
  (is-ellipsis? [this]
    (Pattern/matches re-ellipsis tok))
  (is-number? [this]
    (.startsWith (get-type this) "##number##"))
  (is-initial? [this]
    (Pattern/matches re-initial tok))
  (is-alpha? [this]
    (Pattern/matches re-alpha tok))
  (is-non-punct? [this]
    (Pattern/matches re-non-punct (get-type this))))
      

(defn make-token
  ([tok] (make-token tok {}))
  ([tok params]
    (PunktToken.
      tok
      (.endsWith tok ".")
      (:parastart params)
      (:linestart params)
      (:sentbreak params)
      (:abbr params))))
