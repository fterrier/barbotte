(ns barbotte.command-test
  (:require [barbotte.command :as command]
            [clojure.test :refer [deftest is testing]]))

(deftest parse-command-test
  (testing "Correct command gives no error"
    (is (= [{} nil] (command/parse-command [] [] "")))
    (is (= [{:test ["test"]} nil] 
           (command/parse-command [[:test :list "test"]] ["test"] ""))))
  
  (testing "Error in format"
    (is (= [nil {:error :format-error
                 :type :test
                 :text {:message "Wrong date. Wrong format."
                        :options {:parse-mode :markdown}}}] 
           (command/parse-command [[:test :date "Wrong date."]] ["bullshit"] "Wrong format."))))

  (testing "Error in args"
    (is (= [nil {:error :format-error
                 :type :too-many-args
                 :text {:message "Wrong number of arguments. Wrong format."
                        :options {:parse-mode :markdown}}}]
           (command/parse-command [] ["bullshit"] "Wrong format.")))))
