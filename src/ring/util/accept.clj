(ns ring.util.accept)

(defn best-match
  "Choose best renders given accept header of request"
  ;; TODO: Support wild card matching; hopefully this doesn't
  ;; need to be implemented from scratch.
  [request renders]
  (let [accept (request :accept)
        accept-media (into (sorted-set) (map :media-type accept))
        render-media (into (sorted-set) (keys renders))]
    (first (clojure.set/intersection accept-media render-media))))

(defn fulfill
  "Apply best rendering function to the response. Set the Content-Type header
  to indicate the response body has been appropriately transformed."
  [response media-type render-fn]
  (-> (if (fn? response) (response) response)
      (render-fn)
      (assoc-in [:headers "Content-Type"] media-type)))

(defn fallback
  "Default response when no acceptable content can be produced.
  See https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.7"
  [renders response]
  {:status 406 :body (keys renders)})

(defmacro defaccept [name & renders]
  `(defn ~name [request# response#]
     (let [renders# (hash-map ~@renders)
           best-media# (best-match request# renders#)
           best-render-fn# (renders# best-media# ~fallback)]
       ;; if there is a match, then generate the response
       (if (seq best-media#)
         (fulfill response# best-media# best-render-fn#)
         (fallback renders# response#)))))
