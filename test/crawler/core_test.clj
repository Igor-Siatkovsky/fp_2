(ns crawler.core-test
  (:require [clojure.test :refer :all]
            [crawler.core :refer :all]
            [net.cgrand.enlive-html :as html]
            [org.httpkit.client :as httpkit]))

(deftest test-get-page
  (testing "Processes 404 code"
    (with-redefs-fn {#'httpkit/get (fn [& _] (atom {:status 404}))}
      #(is (=
        [:not-found nil]))))
        (get-page "url")

  (testing "Processes 1111 code as unknown error"
    (with-redefs-fn {#'httpkit/get (fn [& _] (atom {:status 1111}))}
      #(is (=
        [:unknown-error 1111])))))
        (get-page "url")

(deftest test-filter-and-select-links
  (testing "Gets absolute links"
    (is (=
      (filter-and-select-links ["http://onliner.by/index", "relative.html", "/other.html", "https://https.com/options"])
      ["http://onliner.by/index", "https://https.com/options"]))))

(deftest test-fetch-links-from-body
  (testing "Gets all links from html page"
    (let [html-string "<a href=\"http://onliner.by/link\">Link</a>
            <div>div content</div><a href=\"/options/link\">Link</a>"
          html-fragment (html/html-resource (java.io.StringReader. html-string))]
      (is (=
        (fetch-links-from-body html-fragment)
        ["http://onliner.by/link", "/options/link"])))))
