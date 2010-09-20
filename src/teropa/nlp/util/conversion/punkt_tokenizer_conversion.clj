(ns teropa.nlp.util.conversion.punkt-tokenizer-conversion
  "Converts pretrained punkt tokenizers from Python pickle to serialized
   Clojure data structures"
  (:require [clojure.java.shell :as sh])
  (:require [clojure.contrib.json :as json])
  (:require [clojure.contrib.duck-streams :as streams])
  (:require [clojure.pprint :as pp])
  (:import [java.io File])
  (:import [org.apache.commons.io FilenameUtils FileUtils]))

(defn python-command [f]
  (str "import nltk.data\n"
       "import json\n"
       "e = nltk.data.LazyLoader('tokenizers/punkt/" (.getName f) "')\n"
       "print json.dumps({'ortho-context': e._params.ortho_context, 'abbrev_types': list(e._params.abbrev_types), 'collocations': list(e._params.collocations)})\n"))

(defn- data-path []
  (.getAbsolutePath (File. "resources")))

(defn- clojure-file [f]
  (let [res (File.
              (str (FilenameUtils/getFullPath (.getAbsolutePath f))
                   (FilenameUtils/getBaseName (.getAbsolutePath f))
                   ".clj"))]
    (if-not (.exists res)
      (.createNewFile res))
    res))

(defn run []
  (doseq [f (FileUtils/listFiles
              (File. "resources/tokenizers/punkt")
              (into-array ["pickle"])
              false)]
    (println "Converting" f)
    (let [res (sh/sh "python" "-c" (python-command f)
                :env {"NLTK_DATA" (data-path)})
          params (json/read-json (:out res) false)]
      (with-open [w (streams/writer (clojure-file f))]
        (binding [*out* w]
          (pp/pprint {:ortho-context (params "ortho-context")
                      :abbrev-types (apply hash-set (params "abbrev_types"))
                      :collocations (apply hash-set (params "collocations"))}))))))






