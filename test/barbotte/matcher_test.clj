(ns barbotte.matcher-test
  (:require [clojure.test :refer [deftest is testing]]
            [barbotte.matcher :as matcher]))

(deftest matcher-test
  (testing "match-or no match"
    (let [match-fn (matcher/match-or (fn [_] false))]
      (is (not (match-fn [])))))

  (testing "match-or match"
    (let [match-fn (matcher/match-or (fn [_] true))]
      (is (match-fn []))))

  (testing "match-first"
    (let [match-fn (matcher/match-first "first")]
      (is (match-fn {:args ["first"]}))
      (is (not (match-fn {:args ["nfirst"]})))))
  
  (testing "match-first-number"
    (let [match-fn (matcher/match-first-pattern #"^\d*")]
      (is (match-fn {:args ["1."]}))
      (is (match-fn {:args ["10."]}))
      (is (not (match-fn {:args [" 10."]})))
      (is (not (match-fn {:args ["/test"]}))))))
