(ns clorent.decoder
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.string :as str]))

(defn- to->str [xs]
  (apply str xs))

(defn- to->num [[_ & _ :as xs]]
  (read-string (apply str xs)))


(defn- decode-string [[x & xs] acc]
  (if-let [length (and (= \: x) (to->num acc))]
    [(->> xs (take length) to->str) (drop length xs)]
    (recur xs (conj acc x))))

(defn- decode-integer [[x & xs] acc]
  (if (= \e x)
    [(to->num acc) xs] 
    (recur xs (conj acc x))))

(defn- decode-list [xs acc]
  :unsupported)

(defn- decode-dict [xs acc]
  :unsupported)

(defn decode-data [[x & xs :as input]]
  (cond
    (= \d x) (decode-dict xs {})
    (= \l x) (decode-list xs [])
    (= \i x) (decode-integer xs [])
    :else (decode-string (seq input) [])))

(defn decode [^String xs]
  "Decodes torrent string into clojure map."
  (decode-data xs))

