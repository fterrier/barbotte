(ns barbotte.bot-test
  (:require [clojure.test :refer [deftest is testing]]
            [barbotte.bot :as bot]))

(defn telegram-message [text]
  {:update_id 531750612, 
   :message {:message_id 27, 
             :from {:id 86757011,
                    :first_name "François", 
                    :last_name "Terrier",
                    :username "fterrier"}, 
             :chat {:id 86757011,
                    :first_name "François", 
                    :last_name "Terrier",
                    :username "fterrier", 
                    :type "private"}, 
             :date 1468642768, 
             :text text}})

(defn- match-first [text]
  (fn [{:keys [args]}] (= (first args) text)))

(deftest parse-message-add-test
  (testing "Error when command not found"
    (let [send-to-user-fn (fn [response command]
                            (is (nil? command))
                            (is (= :command-not-found (:error response))))]
      (bot/handle-message* [] (telegram-message "garbage") send-to-user-fn)))
  
  (testing "Args are passed correctly"
    (let [handle-fn (fn [{:keys [args]} _]
                      (is (= args ["test" "this" "is" "a" "test"])))]
      (bot/handle-message* [{:match-fn (match-first "test") :handle-fn handle-fn}] (telegram-message " test this is   a  test") nil)))

  (testing "Same matcher takes first one"
    (let [commands [{:match-fn (match-first "test") 
                     :handle-fn (fn [_ _] (is true))}
                    {:match-fn (match-first "test") 
                     :handle-fn (fn [_ _] (is false))}]]
      (bot/handle-message* commands (telegram-message "test") nil))))

(deftest create-bot-test  
  (testing "Bot with good command creates succesfully"
    (let [commands [{:match-fn (match-first "test1") :handle-fn (fn [_ send-fn] (send-fn {:success :test1}))}
                    {:match-fn (match-first "test2") :handle-fn (fn [_ _])}]
          bot      (bot/create-bot commands)
          send-to-user-fn (fn [response command]
                            (is (not (nil? command)))
                            (is (= {:success :test1} response)))]
      (is (not (nil? bot)))
      (bot (telegram-message "test1") send-to-user-fn))))
