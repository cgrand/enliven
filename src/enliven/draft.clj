(ns enliven.draft)

(def todo
  (template "todo.html"
    (dup [:ul#todo :li] :todos
      (content [:label] :description)
      (on :click [checkbox] (move-task [:todo] [:done])))
    (dup :node [:ul#done :li] :for :done
      (content [:label] :description))
    (on :click [checkbox] (move-task [:done] [:todo]))
    (content [:textarea] :draft)
    (on :submit [:form#new-task]
      (fn [state form-data]
        (-> state
          (update-in [:todo] conj {:description (:task form-data)})
          (assoc :draft ""))))))

(let [t (mash
          (dup [:ul#todo :li] :todos  
            (content [:label] :description)
            (on :click [checkbox] (move-task [:todo] [:done])))
          (dup :node [:ul#done :li] :for :done
            (content [:label] :description))
          (on :click [checkbox] (move-task [:done] [:todo]))
          (content [:textarea] :draft)
          (on :submit [:form#new-task]
            (fn [state form-data]
              (-> state
                (update-in [:todo] conj {:description (:task form-data)})
                (assoc :draft "")))))]
  (specialize t (load "todo.html")))



(content [:textarea] :draft)
; this code, when evaluated by clj returns a Transformation
; this code, when evaluated by cljs returns ISomething which
; when passed
(content [:textarea] :draft)



{[....] XXX}

;; a transform emits two kinds of output:
;; * a fn data -> HTML maps
;; * cljs code for fn data -> nodes (DOM or ?)

