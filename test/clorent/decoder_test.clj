(ns clorent.decoder-test
  (:use clojure.test)
  (:use clorent.decoder)
  (:require [clojure.java.io :as io]))

(def torrent-file (io/file
                (io/resource 
                 "taocp.torrent")))

(deftest should_decode_torrent_file
  (let [torrent (decode (slurp torrent-file))]
    (is (contains? torrent :raw))))
