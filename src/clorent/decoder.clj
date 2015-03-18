(ns clorent.decoder
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.string :as str]))

(declare decode-data)

(defn- seq->str [xs]
  (apply str xs))

(defn- seq->num [xs]
  (if (empty? xs) 0
      (read-string (apply str xs))))

(defn raise! [value]
  (throw (Exception. value)))

(defn decode-string [[x & xs] acc]
  (cond
    (nil? x) (raise! "error decoding string")
    (= \: x) (-> acc seq->num (split-at xs) (update-in [0] seq->str))
    :else (recur xs (conj acc x))))

(defn decode-integer [[x & xs] acc]
  (cond
    (nil? x) (raise! "error decoding integer")
    (= \e x) (vector (seq->num acc) (or xs []))
    :else (recur xs (conj acc x))))

(defn decode-list [[x & xs :as input] acc]
  (cond
    (nil? x) (raise! "error decoding list")
    (= \e x) (vector acc (or xs []))
    :else (let [[value rest] (decode-data input)]
            (recur rest (conj acc value)))))

(defn decode-dict [[x & xs :as input] acc]
  (cond
    (nil? x) (vector acc [])
    (= \e x) (vector acc (or xs []))
    :else (let [[key _rest] (decode-data input)
                [value rest] (decode-data _rest)]
            (recur rest (assoc acc key value)))))

(defn decode-data [[x & xs :as input]]
  (condp = x
    \d (decode-dict xs {})
    \l (decode-list xs [])
    \i (decode-integer xs [])
    (decode-string input [])))

(defn decode [^String xs]
  "Decodes torrent string into clojure map."
  (if (empty? xs) {} (first (decode-data xs))))


