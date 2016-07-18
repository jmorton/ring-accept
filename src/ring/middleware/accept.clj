(ns ring.middleware.accept
  "Functions for parsing Accept header media-ranges."
  (:require [clojure.string :as str]
            [ring.util.parsing :as p]))

(def ^:private re-wildcard #"\*")

;; Parses quality portion of a media range, e.g. ;q=0.9
(def ^:private re-qvalue #"0(?:\.\d{0,3})?|1(?:\.0{0,3})?")

(def ^:private re-accept-params
  (re-pattern (str "\\s*;\\s*" "q=(" re-qvalue ")")))

;; Parses the extension portion of a media range, e.g. ;foo=1;bar=2
(def ^:private re-parameter
  (re-pattern (str "\\s*;\\s*" p/re-token "=" p/re-value)))

;; Splits each extension into match/key/value.
(def ^:private re-extensions
  (re-pattern "(?<=;)([^=]+)=([^;]+)"))

(def ^:private re-vendor
  (re-pattern "([^\\+]+)"))

;; Non-standard, not specified by an RFC.
(def ^:private re-version
  (re-pattern "v([0-9\\.]+)"))

;; Non-standard
(def ^:private re-format
  (re-pattern ".+\\+(.+)"))

;; Technically, this pattern is incorrect because it matches
;; an erroneous media-range such as "*/plain", but a more
;; complicated pattern introduces complexities when parsing
;; ranges and subranges.
(def ^:private re-media-range
  (re-pattern (str "((" re-wildcard "|" p/re-token ")"
                   "/(" re-wildcard "|" p/re-token "))"
                   "((?:" re-parameter ")*?)")))

;; Parses one entire media-range.
(def ^:private re-accept-value
  (re-pattern (str "(?:" re-media-range ")"
                   "(?:(" re-accept-params ")"
                   "(" re-parameter ")*)?")))

;; Parses the entire accept header, splitting multiple media-ranges.
(def ^:private re-accept-header
  (re-pattern (str "(?<=^|,)\\s*(" re-media-range ")\\s*(?:,|$)")))

(defn- parse-accept
  ""
  [header-value]
  (let [[_ media-type type subtype parameters _ _ q] (re-matches re-accept-value header-value)]
    {:media-type media-type
     :type type
     :subtype subtype
     :parameters parameters
     :specificity 0
     :quality (Double/parseDouble (or q "1"))}))

(defn- parse-header
  ""
  [header]
  (map second (re-seq re-accept-header header)))

;; According to RFC7231, values are case insensitive.
;; In order to support consistent matching, strings
;; are converted to lower case.
;; https://tools.ietf.org/html/rfc7231#section-3.1.1.1
(defn normalize
  "Make media-range consistently lower-case."
  [media-range]
  (clojure.string/lower-case media-range))

(defn +specificity
  "Used when sorting indidual accept values."
  [accept]
  (assoc accept :specificity
         (cond->> 0
           (not-empty (accept :extensions)) (+ 4)
           (not= (accept :subtype) "*") (+ 2)
           (not= (accept :type) "*") (+ 1))))

(defn +extensions
  "Convert a single extensions string into a hash-map of strings."
  [accept]
  (if-let [params (:parameters accept)]
    (->> params
         (re-seq re-extensions)
         (map rest)
         (map vec)
         (reduce conj {})
         (assoc accept :extensions))
    (assoc accept :extensions {})))

(defn +vendor
  ""
  [accept]
  (assoc accept :vendor (last (re-find re-vendor (accept :subtype)))))

(defn +version
  "Non standard. Parse v### from anywhere in media-range."
  [accept]
  (assoc accept :version (last (re-find re-version (accept :subtype)))))

(defn +format
  ""
  [accept]
  (assoc accept :format (last (re-find re-format (accept :subtype)))))

(defn prioritize
  "Sort media-types by quality and specificity, most to least."
  [accepts]
  (sort-by (juxt :quality :specificity) #(compare %2 %1) accepts))

(def accept-xf (comp (map normalize)
                     (map parse-accept)
                     (map +extensions)
                     (map +specificity)
                     (map +vendor)
                     ;; non-standard media-range properties
                     (map +version)
                     (map +format)))

(defn accept-request
  ""
  [request]
  (if-let [header (get-in request [:headers "accept"])]
    (let [values (parse-header header)
          accept (prioritize (sequence accept-xf values))]
      (assoc request :accept accept))))

(defn wrap-accept
  ""
  [handler]
  (comp handler accept-request))
