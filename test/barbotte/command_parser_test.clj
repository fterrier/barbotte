(ns barbotte.command-parser-test
  (:require [barbotte.command-parser :as parser]
            [clj-time
             [core :as t]
             [format :as f]
             [predicates :as pr]]
            [clojure.test :refer [deftest is testing]]))

(def test-date (f/parse (f/formatter "dd-MM-yyyy" (t/default-time-zone)) "27-11-2015"))

(deftest parse-message-add-test
  (testing "Correct message is parsed properly"
    (let [[command error] (parser/parse-command-chunks 
                           [:list :timespan :date] 
                           ["15,16,17" "17:00-21:00" "27-11-2015"])]
      (is (= [["15" "16" "17"] [61200 75600] test-date] command))
      (is (nil? error))))
  
  (testing "Correct message is parsed properly - hours in HH'h'"
    (let [[command error] (parser/parse-command-chunks [:timespan] ["17h-21h"])]
      (is (= [[61200 75600]] command))
      (is (nil? error))))

  (testing "Correct message is parsed properly - hours in HH'h'mm"
    (let [[command error] (parser/parse-command-chunks [:timespan] ["17h00-21h30"])]
      (is (= [[61200 77400]] command))
      (is (nil? error))))

  (testing "Correct message is parsed properly - date in dd/MM/yyyy"
    (let [[command error] (parser/parse-command-chunks [:timespan] ["17h00-21h30"])]
      (is (= [[61200 77400]] command))
      (is (nil? error))))

  (testing "Correct message is parsed properly - date in dd/MM"
    (let [[command error] (parser/parse-command-chunks [:date] ["27/11/2015"])]
      (is (= [test-date] command))
      (is (nil? error))))

  ;; (testing "Correct message is parsed properly - date in dd/MM"
  ;;   (let [[command error] (parser/parse-command-chunks ["15,16,17" "17h00-21h30" "27/11"])]
  ;;     (is (= (test-result 61200 77400)
  ;;            command))
  ;;     (is (nil? error))))

  (testing "Correct message is parsed properly - date <monday>"
    (let [[command error] (parser/parse-command-chunks [:date] ["monday"])]
      (is (pr/monday? (first command)))
      (is (t/after? (first command) (t/now)))
      (is (nil? error))))

  (testing "Correct message is parsed properly - date <on monday>"
    (let [[command error] (parser/parse-command-chunks [:date] ["monday"])]
      (is (pr/monday? (first command)))
      (is (t/after? (first command) (t/now)))
      (is (nil? error))))

  (testing "Error - too little arguments"
    (let [[command error] (parser/parse-command-chunks [:date :timespan] ["monday"])]
      (is (= {:error :format-error :type :not-enough-args} error))))
  
  (testing "Error - too many arguments"
    (let [[command error] (parser/parse-command-chunks [:list] ["10,12" "monday"])]
      (is (= {:error :format-error :type :too-many-args} error))))

  (testing "Bad timespan"
    (let [[command error] (parser/parse-command-chunks [:timespan] ["test"])]
      (is (nil? error))
      (is (nil? (first command)))))

  ;(testing "Bad courts)
  ;(testing "Bad date)
  )

