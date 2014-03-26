(ns schema-gen.core
  "Functions for generating test data from schemas."
  (:require [four.stateful :as four]
            [re-rand :refer [re-rand]]
            [schema.core :as sch]
            [clojure.test.check.generators :as gen]))

(defn ^:private re-randify-regex
  "schema requires ^$ while re-rand forbids them"
  [re]
  (let [s (str re)]
    (if (re-matches #"\^.*\$" s)
      (re-pattern (subs s 1 (dec (count s))))
      re)))

(declare schema->gen)

(defn record->gen
  "record should be a map from keys to schemas."
  [record]
  (apply gen/hash-map (mapcat (fn [[k schema]]
                                [k (schema->gen schema)])
                              record)))

(defprotocol GenSchema
  (schema->gen* [this]))

(defn schema->gen
  "Oh man here we go!"
  [schema]
  ;; TODO: other kinds of keys, optional keys, etc.

  ;; I'm doing the keyword? check here and elsewhere because I don't
  ;; know of another simple way to distingush regular keys from a
  ;; singular key representing the schema of the keys of a homogeneous
  ;; map
  (cond
    (= schema sch/Keyword)
    gen/keyword

    (= schema sch/Bool)
    (gen/elements [true false])

    (= schema sch/Int)
    gen/int

    (= schema sch/Num)
    (gen/fmap float gen/ratio)

    (satisfies? GenSchema schema)
    (schema->gen* schema)

    (map? schema)
    (gen/fmap (partial apply merge)
              (apply gen/tuple
                (record->gen (filter (comp keyword? first) schema))
                (for [[k v] schema :when (not (keyword? k))]
                  (gen/map (schema->gen k) (schema->gen v)))))

    (= schema sch/Str)
    gen/string-ascii ; bad idea to exclude unicode from tests?

    (and
      (vector? schema)
      (= (count schema) 1))
    (gen/vector (schema->gen (first schema)))

    :else
    (throw (ex-info "Unknown schema format in schema->gen!"
                    {:schema schema}))))

(extend-type java.util.regex.Pattern
  GenSchema
  (schema->gen* [schema]
    (let [re (re-randify-regex schema)]
      {:gen (fn [r _size]
              (binding [four/*rand* r]
                (gen/rose-pure (re-rand re))))})))

(extend-type schema.core.EqSchema
  GenSchema
  (schema->gen* [this]
    (gen/return (:v this))))

(extend-type schema.core.AnythingSchema
  GenSchema
  (schema->gen* [_] gen/any-printable))

(extend-type schema.core.EnumSchema
  GenSchema
  (schema->gen* [this]
    (gen/elements (:vs this))))

(defmacro no-impl [t]
  `(extend-type ~t
     GenSchema
     (schema->gen* [_#]
       (throw (ex-info "Schema type not implemented"
                       {:type ~t})))))

(no-impl schema.core.Predicate)
(no-impl schema.core.Protocol)
