(ns clorent.decoder
  (:require [clojure.core.match :refer [match]])
  (:require [clorent.sugar :as sugar])
  (:require [clojure.string :as str]))


(defn- to-num [xs]
  (read-string (apply str xs)))

(defn- decode-string [[x & xs] acc]
  (if (= \: x)
    (let [[chars rest] (split-at (to-num acc) xs)]
      [(apply str chars) rest])
    (recur xs (conj acc x))))

(defn- decode-integer [[x & xs] acc]
  (if (= \e x) [(read-string (apply str acc)) xs] 
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
    :else (decode-string input [])))

(defn decode [^String xs]
  "Decodes torrent string into clojure map."
  (decode-data xs))

