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

(defn fallback
  "Default response when no acceptable content can be produced"
  [response]
  (merge response {:status 406}))

(defmacro defaccept [name & renders]
  `(defn ~name [request# response#]
     (let [renders# (hash-map ~@renders)
           best-media# (best-match request# renders#)
           best-render-fn# (renders# best-media# ~fallback)]
       (->> (assoc-in response# [:headers "Content-Type"] best-media#)
            (best-render-fn#)))))

(assoc-in {} [:a "b"] 1)
