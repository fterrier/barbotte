(ns barbotte.matcher
  (:require [clojure.string :as str]))

(defn match-or [& fns]
  (fn [message] (some (fn [match-fn] (match-fn message)) fns)))

(defn match-first [text]
  (fn [{:keys [args]}] (= (first args) text)))

(defn match-first-pattern [regex]
  (let [matcher (re-pattern regex)]
    (fn [{:keys [args]}] 
      (not (str/blank? (re-find matcher (first args)))))))
