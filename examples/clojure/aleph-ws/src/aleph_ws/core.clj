(ns aleph-ws.core
  (:use lamina.core aleph.http))

(defn hello-world [channel request]
  (enqueue channel
    {:status 200
     :headers {"content-type" "text/html"}
     :body (slurp "html/index.html")}))

(def broadcast-channel (channel))
;; (enqueue broadcast-channel "stuff")

(defn chat-handler [ch handshake]
  (receive ch
    (fn [name]
      (printf "in the anon-fn inside chat-handler name=%s\n" name)
      (siphon (map* (fn [arg]
                      (printf "in the siphon-fn's body, arg=%s\n" arg)
                      (str name ": " arg))
                    ch)
              broadcast-channel)
      (siphon broadcast-channel ch))))


(comment
  (def *server* (start-http-server hello-world {:port 8080}))
  (*server*)
  (def *ws-server* (start-http-server chat-handler {:port 8081 :websocket true}))

  (*ws-server*)

  (def a (channel))
  (def b (channel))

  (siphon (map* inc a) b)

  (enqueue a 1)
  (receive b (fn [x] (printf "got an '%s'\n" x)))

 )
