(ns enliven.html.render)

(defn render [node f acc]
  (cond
    (string? node) (f acc node)
    (:tag node) (render-element node f acc)
    (comment? node) (render-comment node f acc)
    :else acc))

(defn render-str [node]
  (str (render node (fn [^StringBuilder sb s]
                          (.append sb s))
         (StringBuilder.))))



(defn pre-render [node plan]
  (cond 
    (empty? plan) [:static (render node str "")]
    (string? node) (fn [data f acc]
                     DO TRANSFO AND RENDER)
    
    (:tag node) 
    (let [plans (narrow-plans plan [:tag :attrs :content])]
      (if (::unexpected plans)
        (fn [data f acc]
          DO TRANSFO AND RENDER)
        (if (:content plans)
          )))
    ))

