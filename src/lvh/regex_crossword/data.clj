(ns lvh.regex-crossword.data
  (:require [cheshire.core :as json]
            [clj-http.lite.client :as http]
            [camel-snake-kebab.core :as csk]
            [clojure.java.io :as io]))

(def ff-req-headers
  "Pretend to be a browser closely enough to fool CloudFlare."
  {"Accept"	"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
   "Accept-Language"	"en-US,en;q=0.5"
   "User-Agent" "Mozilla/5.0 (X11; Linux x86_64; rv:69.0) Gecko/20100101 Firefox/69.0"})

(defn ^:private get!
  [url]
  (-> (http/get url {:headers ff-req-headers}) :body (json/parse-string csk/->kebab-case-keyword)))

(defn get-player-puzzle!
  "Download a specific player puzzle."
  [id]
  (get! (str "https://regexcrossword.com/api/puzzles/" id)))

(defn get-builtin-puzzles!
  "Download the game's builtin challenges."
  []
  (get! "https://regexcrossword.com/data/challenges.json"))

(def builtin-puzzles
  "A cached version of the builtin puzzles. Will attempt to load as a resource and
  download as a fallback."
  (delay
    (-> "challenges.json" io/resource io/reader (json/parse-stream csk/->kebab-case))
    (get-builtin-puzzles!)))
