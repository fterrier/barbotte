(ns barbotte.bot
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]))

(defn- parse-command [text]
  (str/split (str/trim text) #"\s+"))

;; TODO handle edited-message
(defn- parse-message [{:keys [message] :as data}]
  "Takes a telegram message and parses it to {:user ... :command-key ... :error ...}"
  (log/info "Parsing message" message)
  (let [user       (:from message)
        args       (parse-command (:text message))
        message-id (:message_id message)]
    {:user user
     :args args
     :message-id message-id}))

(defn- find-command [commands message]
  (some (fn [{:keys [match-fn] :as command}] 
          (when (match-fn message) command)) commands))

(defn handle-message* [commands data send-to-user-fn]
  (log/info "Incoming message for bot" data)
  (let [message     (parse-message data)
        command     (find-command commands message)
        response-fn (fn [response] (send-to-user-fn response command))]
    (log/info "Got command for message: " message command)
    (if-not (nil? command)
      ((:handle-fn command) message response-fn)
      (send-to-user-fn {:error :command-not-found} nil))))

(defn- send-to-user-with-log-fn [send-to-user-fn response command]
  (log/info "Sending response to user" response)
  (send-to-user-fn response command))

(defn create-bot [commands]
  "Creates a bot with the given commands. Commands is a list of {:match-fn ... :handle-fn ...}}.
   - match-fn: a function that returns true if the command should match
   - handler-fn: a function that takes as args [user args send-to-user-fn]"
  (fn [data send-to-user-fn]
    (handle-message* commands data (partial send-to-user-with-log-fn send-to-user-fn))))

