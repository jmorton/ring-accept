(ns ring.middleware.accept-test
  (:require [clojure.test :refer :all]
            [ring.middleware.accept :as accept]))

(deftest parse-accept-test
  (testing "single media-range without quality"
    (let [media-range "text/html"
          request {:headers {"accept" media-range}}
          results (-> request accept/accept-request :accept first)]
      (is (= 1.0 (results :quality)))
      (is (= "text" (results :type)))
      (is (= "html" (results :subtype)))
      (is (= "text/html" (results :media-type)))))
  (testing "single media-range with quality"
    (let [media-range "text/html;q=0.5"
          request {:headers {"accept" media-range}}
          results (-> request accept/accept-request :accept first)]
      (is (= 0.5 (results :quality)))
      (is (= "text" (results :type)))
      (is (= "html" (results :subtype)))
      (is (= "text/html" (results :media-type)))))
  (testing "media-range quality sorting"
    (let [media-range "text/baz;q=0.5, text/foo, text/bar;q=0.9"
          request {:headers {"accept" media-range}}
          results (:accept (accept/accept-request request))]
      (is (= "text/foo" (:media-type (nth results 0))))
      (is (= "text/bar" (:media-type (nth results 1))))
      (is (= "text/baz" (:media-type (nth results 2))))))
  (testing "media-range specificity sorting"
    (let [media-range "text/*, text/html, text/html;level=1, */*"
          request {:headers {"accept" media-range}}
          results (:accept (accept/accept-request request))]
      (is (= {"level" "1"} (:extensions (nth results 0))))
      (is (= "text/html" (:media-type (nth results 1))))
      (is (= "text/*" (:media-type (nth results 2))))
      (is (= "*/*" (:media-type (nth results 3))))))
  (testing "media-range quality and specificity sorting"
    (let [media-range "text/*;q=0.9, text/foo;q=0.9, text/bar"
          request {:headers {"accept" media-range}}
          results (:accept (accept/accept-request request))]
      (is (= "text/bar" (:media-type (nth results 0))))
      (is (= "text/foo" (:media-type (nth results 1))))
      (is (= "text/*" (:media-type (nth results 2))))))
  (testing "equivalance irrespective of case"
    (let [media-range "Text/HTML;Charset=UTF-8"
          request {:headers {"accept" media-range}}
          results (first (:accept (accept/accept-request request)))]
      (is (= "text/html" (:media-type results)))
      (is (= {"charset" "utf-8"} (:extensions results))))))
