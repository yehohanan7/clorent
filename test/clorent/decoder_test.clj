(ns clorent.decoder-test
  (:use clojure.test)
  (:use clorent.decoder)
  (:require [clojure.java.io :as io]))

(def test-torrent (-> "taocp.torrent" io/resource io/file slurp))


(deftest should-decode-string
  (is (= ["abcde" []] (decode-data "5:abcde")))
  (is (= ["abcde" (seq "fgh")] (decode-data "5:abcdefgh"))))


(deftest should-decode-data
  (is (= [57 []] (decode-data "i57e")))
  (is (= [57 (seq "2:ab")] (decode-data "i57e2:ab"))))


(deftest should-decode-list
  (is (= [["ab" "xyz" 57] []] (decode-data "l2:ab3:xyzi57ee"))))
