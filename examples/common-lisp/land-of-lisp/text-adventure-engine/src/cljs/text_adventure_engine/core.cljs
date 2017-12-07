(ns text-adventure-engine.core)

(defn mount-components []
  (let [content (js/document.getElementById "app")]
    (while (.hasChildNodes content)
      (.removeChild content (.-lastChild content)))
    (.appendChild content (js/document.createTextNode "Welcome to text-adventure-engine"))))

(defn init! []
  (mount-components))
