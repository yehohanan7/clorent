(ns clorent.decoder
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.string :as str]))

(declare decode-data)

(defn- to-str [xs]
  (apply str xs))

(defn- to-num [xs]
  (if (empty? xs) 0
      (read-string (apply str xs))))

(defn- apply-first [[x y] f]
  (vector (f x) y))

(defn decode-string [[x & xs] acc]
  (if (= \: x)
    (-> acc to-num (split-at xs) (apply-first to-str))
    (recur xs (conj acc x))))

(defn decode-integer [[x & xs] acc]
  (if (= \e x)
    [(to-num acc) (or xs [])]
    (recur xs (conj acc x))))

(defn decode-list [[x & xs :as input] acc]
  (if-let [[value rest]  (and (not= \e x) (decode-data input))] 
    (recur rest (conj acc value))
    [acc (or xs [])]))

(defn decode-dict [[x & xs :as input] acc]
  (if (= \e x) [acc (or xs [])]
      (let [[key rest*] (decode-data input)
            [value rest] (decode-data rest*)]
        (recur rest (assoc acc key value)))))

(defn decode-data [[x & xs :as input]]
  (condp = x
    \d (decode-dict xs {})
    \l (decode-list xs [])
    \i (decode-integer xs [])
    (decode-string input [])))

(defn decode [^String xs]
  "Decodes torrent string into clojure map."
  (decode-data xs))

