(ns clorent.decoder
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.string :as str]))


(defn string-decoder []
  (fn [step]
    (let [prefix (volatile! "") string (volatile! "")]
      (fn [r x]
        (cond
          (= \: x) (initialize the string accumulator)
          (constructing string?) (append the character to the string)
          (string constructed?) (step r (constructed string)))))))

(def decoder
  (comp
   (string-decoder)
   (integer-decoder)
   (list-decoder)
   (dict-decoder)))

(defn decode [xs]
  (transduce decoder {} xs))


(declare decode-data)

(defn- to-str [xs]
  (apply str xs))

(defn- to-num [xs]
  (read-string (apply str xs)))

(defn decode-string [[x & xs] acc]
  (if-let [length (and (= \: x) (to-num acc))]
    (vector (to-str (take length xs)) (drop length xs))
    (recur xs (conj acc x))))

(defn decode-integer [[x & xs] acc]
  (if (= \e x)
    [(to-num acc) (or xs [])]
    (recur xs (conj acc x))))

(defn decode-list [[x & xs :as input] acc]
  (if-let [[value rest]  (and (not= \e x) (decode-data input))] 
    (recur rest (conj acc value))
    [acc (or xs [])]))

(defn- decode-dict [[x & xs :as input] acc]
  (if (= \e x) [acc (or xs [])]

      (iterate #() [input acc] )
      
      (let [[key rest*] (decode-data input)
            [value rest] (decode-data rest*)])
      (recur rest (assoc acc key value))))

(defn decode-data [[x & xs :as input]]
  (condp = x
    \d (decode-dict xs {})
    \l (decode-list xs [])
    \i (decode-integer xs [])
    (decode-string input [])))

(defn decode [^String xs]
  "Decodes torrent string into clojure map."
  (decode-data xs))

