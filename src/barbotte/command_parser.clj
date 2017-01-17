(ns barbotte.command-parser
  (:require [clj-time
             [core :as t]
             [format :as f]]
            [clojure.string :as str]
            [duckling.core :as duckling]))

(def dmy-1 (f/formatter "dd-MM-yyyy" (t/default-time-zone)))
(def dmy-2 (f/formatter "dd/MM/yyyy" (t/default-time-zone)))

(def hour-minute (f/formatter (t/default-time-zone) 
                              "HH'h'" "HH'h'mm" "HH:mm" "HHmm"))

(defn- parse-time-nil [formatter unparsed-time]
  (try
    (f/parse formatter unparsed-time)
    (catch Exception e)))

(defn- parse-time [formatters unparsed-time]
  "Try the formatters in order until one does not return nil and returns that value."
  (reduce #(let [parsed-time (parse-time-nil %2 unparsed-time)]
             (if parsed-time (reduced parsed-time) nil)) nil formatters))

(defn- number-of-seconds [unparsed-time]
  (let [ref-time    (f/parse hour-minute "00:00")
        parsed-time (parse-time [hour-minute] unparsed-time)]
    (if-not parsed-time nil
      (t/in-seconds (t/interval ref-time parsed-time)))))

(defn- parse-timespan [hours-text]
  (let [times      (str/split hours-text #"-")
        start-time (number-of-seconds (first times))
        end-time   (number-of-seconds (second times))]
    (if
      (not= (count times) 2) nil
      [start-time end-time])))

(defn- parse-int [string]
  (try
    (Integer/parseInt string)
    (catch Exception e)))

(defn- parse-known-expressions [string]
  (let [parsed-times (duckling/parse :en$core string [:time])
        first-time   (first (filter #(= :day (:grain (:value %))) parsed-times))]
    (when first-time
      (t/to-time-zone 
       (f/parse (f/formatters :date-time) 
                (:value (:value first-time)))
       (t/default-time-zone)))))

(defn- parse-date [string]
  (let [parsed-time (parse-time [dmy-1 dmy-2] string)]
    (if parsed-time parsed-time
        (parse-known-expressions string))))

(defn- parse-list [list-text]
  (let [items (str/split list-text #",")]
    (->> items
         (remove nil?))))

(def ^:private init-parser! (memoize (fn [] (duckling/load! {:languages ["en"]}))))

(defn- parse-command-chunk [format-arg args]
  "Returns [parsed-arg remaining-args]."
  (case format-arg
    :list     [(parse-list (first args)) (rest args)]
    :timespan [(parse-timespan (first args)) (rest args)]
    :date     [(parse-date (str/join " " args)) []]
    :default  [nil args]))

(defn parse-command-chunks [format-args args]
  "Given a list of expected formats and arguments, returns the parsed arguments in the 
   given order. For each arg in format-args, returns {:value ... :type ...} where :value is nil
   if the arg could not be properly parsed. 
   Returns a [parsed-args errors] vector."
  (init-parser!)
  (let [[parsed-args remaining-args] 
        (reduce (fn [[parsed-args remaining-args] format-arg]
                  (if (empty? remaining-args)
                    [parsed-args remaining-args]
                    (let [[parsed-arg remaining-args] (parse-command-chunk format-arg remaining-args)]
                      [(conj parsed-args parsed-arg) remaining-args])))
                [[] args] format-args)]
    (cond 
      (not (empty? remaining-args)) [parsed-args {:error :format-error :type :too-many-args}]
      (not (= (count format-args) (count parsed-args))) [parsed-args {:error :format-error :type :not-enough-args}]
      :else [parsed-args nil])))

