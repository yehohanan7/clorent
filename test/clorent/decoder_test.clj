(ns clorent.decoder-test
  (:use clojure.test)
  (:use clorent.decoder)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def test-torrent (-> "taocp.torrent" io/resource io/file slurp str/trim))


(deftest decode-test
  (is (= "abcde" (decode "5:abcde")))
  (is (= 57 (decode "i57e")))
  (is (= {57 "ab"} (decode "di57e2:abe")))
  (is (= {"foo" "ab" "bar" ["mm" 7]} (decode "d3:foo2:ab3:barl2:mmi7eee"))))

(deftest decode-torrent
  (decode test-torrent))
