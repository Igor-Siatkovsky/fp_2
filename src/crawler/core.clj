(ns crawler.core
  (:gen-class)
  (:require [org.httpkit.client :as httpkit]
            [net.cgrand.enlive-html :as html]))

(defn filter-and-select-links
  [list-link]
  (->> list-link
    (filter identity)
    (filter #(.startsWith % "http"))))

(defn make-node
  [value]
  (atom {:value value :children []}))

(defn fetch-links-from-body
  [structured-body]
  (let [a-tags (html/select structured-body [:a])
        list-link (map #(get-in % [:attrs :href]) a-tags)]
    list-link))

(defn note
  [notification point]
  (let [node (make-node notification)]
    (swap! point assoc :children (conj (@point :children) node))
    node))

(defn get-page
  [url]
  (let [http-response @(httpkit/get url {:follow-redirects false :throw-exceptions false})
        status-code (http-response :status)]
    (if (= 200 status-code)
      [:ok (http-response :body)]
      (if (= status-code 404)
        [:not-found nil]
        (if (contains? #{301 302 307} status-code)
          [:redirect (get-in http-response [:headers :location])]
          [:unknown-error status-code])))))

(defn add-new-log
  [tree-structure url & rest] (note (apply str url "    " rest) tree-structure))

(defn process-page
  [tree-structure url body max-depth current-depth]
  (try (let [
             structured-body (html/html-resource (java.io.StringReader. body))
             list-link (filter-and-select-links (fetch-links-from-body structured-body))
             tree-structure (add-new-log tree-structure url "ok " (count list-link) " links")
             ]
            [tree-structure list-link])
    (catch java.lang.ClassCastException e [tree-structure []])))

(defn show-tree
  ([log-root] (println)
    (show-tree (@log-root :children) 0))
  ([nodes level] (let [indent (apply str (repeat (* level 4) " "))]
      (doseq [node nodes]
        (println indent (@node :value))
        (show-tree (@node :children) (+ level 1))))))

(defn handle-redirect-request
  [tree-structure url redirect-url]
  (let [redirect-url (first (filter-and-select-links [redirect-url]))]
    (if redirect-url
      [(add-new-log tree-structure url "redirect" " " redirect-url) url])))

(declare parse)

(defn process-http-code [tree-structure url max-depth current-depth code page-content]
  (if (= code :ok)
    (let [[tree-structure list-link] (process-page tree-structure url page-content max-depth current-depth)]
      (doall (pmap (fn [link]
                      (parse tree-structure link max-depth (inc current-depth)))
                      list-link)))
    (if (= code :not-found)
      (add-new-log tree-structure url "404")
      (if (= code :redirect)
        (let [redirect-url page-content
              [tree-structure url] (handle-redirect-request tree-structure url redirect-url)]
          (if url
            (parse tree-structure redirect-url max-depth (inc current-depth))))
        (if (= code :unknown-error)
          (add-new-log tree-structure url "unknown error"))))))

(defn parse
  [tree-structure url max-depth current-depth]
  (if (>= max-depth current-depth)
    (let [[code page-content] (get-page url)]
      (process-http-code tree-structure url max-depth current-depth code page-content))))

(defn parse-and-fetch-urls-from-file
  [path]
  (with-open [r (clojure.java.io/reader path)] (doall (line-seq r))))

(defn prepare-parsing
  [tree-structure links depth]
  (doall (pmap (fn [url] (parse tree-structure url depth 1)) links))
  (show-tree tree-structure))

(defn -main
  [file-name depth]
  (prepare-parsing (make-node :root) (parse-and-fetch-urls-from-file file-name) (Integer/parseInt depth))
  (shutdown-agents))
