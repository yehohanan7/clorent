(ns clorent.decoder
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.string :as str]))

(defn- to-str [xs]
  (apply str xs))

(defn- to-num [xs]
  (read-string (apply str xs)))

(defn decode-string [[x & xs] acc]
  (if (= \: x)
    [(to-str (take (to-num acc) xs)) (drop (to-num acc) xs)]
    (recur xs (conj acc x))))

(defn decode-integer [[x & xs] acc]
  (if (= \e x)
    [(to->num acc) (or xs [])]
    (recur xs (conj acc x))))

(defn decode-list [[x & xs :as input] acc]
  (if-let [[value rest]  (and (not= \e x) (decode-data input))] 
    (recur rest (conj acc value))
    [acc xs]))

(defn- decode-dict [xs acc]
  :unsupported)

(defn decode-data [[x & xs :as input]]
  (condp = x
    \d (decode-dict xs {})
    \l (decode-list xs [])
    \i (decode-integer xs [])
    (decode-string input [])))

(defn decode [^String xs]
  "Decodes torrent string into clojure map."
  (decode-data xs))

