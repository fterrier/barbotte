(ns barbotte.command
  (:require [barbotte.command-parser :as parser]))

(defn- parse-command-chunks [type-format-args args]
  "Given a list of [<type> <format>] args and a list of args, returns the given command."
  (let [format-args         (map second type-format-args)
        type-args           (map first type-format-args)
        [parsed-args error] (parser/parse-command-chunks format-args args)]
    (if error
      [nil error]
      (reduce (fn [[command error] [type value]]
                (if (nil? value) 
                  (reduced [nil {:error :format-error :type type}])
                  [(assoc command type value) nil]))
              [{} nil] (map vector type-args parsed-args)))))

(defn- get-format-error-message [type type-message-map format-message]
  {:message (str 
             (case type
               :too-many-args   "Wrong number of arguments."
               :not-enough-args "Wrong number of arguments."
               (get type-message-map type))
             " " format-message)
   :options {:parse-mode :markdown}})

(defn parse-command [type-format-message-args args format-message]
  "Parses a list of args given a format, error message and type 
   type-format-message-args [<type> <format> <message>] and a format-message.
   Allowed types given by command-parser."
  (let [[command error] (parse-command-chunks type-format-message-args args)
        error           (when error 
                          (assoc error :text 
                                 (get-format-error-message 
                                  (:type error)
                                  (->> type-format-message-args
                                       (map (fn [[type _ message]] [type message]))
                                       (into {}))
                                  format-message)))]
    [command error]))
