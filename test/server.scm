(use gauche.test)
(use gauche.net)
(use gauche.uvector)
(use gauche.process)
(use gauche.threads)
(use srfi-1)
(test-start "nrepl.server / nrepl.client")
(use nrepl.bencode)
(use nrepl.server)
(use nrepl.client)
(test-module 'nrepl.server)
(test-module 'nrepl.client)

(define (find-key rs key) (any (cut dict-ref <> key) rs))
(define (last-status rs) (dict-ref (last rs) "status"))

(define server #f)
(define port #f)
(define client #f)
(define session #f)
(define s1 #f)
(define s2 #f)
(define raw #f)
(define raw-in #f)
(define raw-out #f)

(define (raw-read-all n)
  (let loop ([buf (make-u8vector 0)])
    (receive (vs rest) (bencode-decode-all buf)
      (if (>= (length vs) n)
        vs
        (loop (u8vector-append buf (read-uvector <u8vector> 4096 raw-in)))))))

;; The server threads evaluate `define` forms; doing that while the main
;; thread is still inside `load` deadlocks in Gauche, so everything runs
;; from `main`, which gosh calls once the script has been loaded.
(define (main args)
  (set! server (start-nrepl-server 0))
  (set! port (nrepl-server-port server))
  (test* "server picked a free port" #t (> port 0))

  (test-section "client over TCP")
  (set! client (nrepl-connect port))
  (set! session (nrepl-clone client))
  (test* "clone returns a uuid" #t (boolean (#/^[0-9a-f-]{36}$/ session)))

  (test* "eval returns value and done" '("\"hi nrepl\"" ("done"))
         (let1 rs (nrepl-eval client "(define (greet n) (string-append \"hi \" n)) (greet \"nrepl\")" session)
           (list (find-key rs "value") (last-status rs))))
  (test* "state persists across evals" "\"hi again\""
         (find-key (nrepl-eval client "(greet \"again\")" session) "value"))
  (test* "out is delivered" "printed"
         (find-key (nrepl-eval client "(display \"printed\")" session) "out"))
  (test* "errors are delivered" #t
         (boolean (#/boom/ (find-key (nrepl-eval client "(error \"boom\")" session) "err"))))
  (test* "describe" '("clone" "close" "describe" "eval")
         (dict-keys (find-key (nrepl-send client (bencode-dict "op" "describe")) "ops")))
  (test* "unknown op" '("error" "unknown-op" "done")
         (last-status (nrepl-send client (bencode-dict "op" "nope"))))
  (test* "close" '("session-closed" "done") (last-status (nrepl-close client session)))
  (test* "eval after close" '("error" "unknown-session" "done")
         (last-status (nrepl-eval client "1" session)))

  (test-section "sessions are isolated")
  (set! s1 (nrepl-clone client))
  (set! s2 (nrepl-clone client))
  (nrepl-eval client "(define secret 1)" s1)
  (test* "binding invisible from another session" #t
         (boolean (#/unbound variable/ (find-key (nrepl-eval client "secret" s2) "err"))))

  (test-section "message framing")
  ;; Raw socket: two messages in one packet, then one message split into pieces.
  (set! raw (make-client-socket 'inet "127.0.0.1" port))
  (set! raw-in (socket-input-port raw))
  (set! raw-out (socket-output-port raw))

  (let1 two (u8vector-append
             (bencode-encode (bencode-dict "id" "a" "op" "eval" "session" s1 "code" "1"))
             (bencode-encode (bencode-dict "id" "b" "op" "eval" "session" s1 "code" "2")))
    (write-uvector two raw-out) (flush raw-out)
    (test* "two messages in one packet are both answered" '("1" "2")
           (let1 rs (raw-read-all 4)
             (list (dict-ref (find (^r (equal? (dict-ref r "id") "a")) rs) "value")
                   (dict-ref (find (^r (equal? (dict-ref r "id") "b")) rs) "value")))))

  (let1 one (bencode-encode (bencode-dict "id" "c" "op" "eval" "session" s1 "code" "(* 6 7)"))
    (let1 n (u8vector-length one)
      (write-uvector (u8vector-copy one 0 (quotient n 2)) raw-out) (flush raw-out)
      (sys-nanosleep 50000000)
      (write-uvector (u8vector-copy one (quotient n 2) n) raw-out) (flush raw-out))
    (test* "a message split across packets is reassembled" "42"
           (find-key (raw-read-all 2) "value")))

  (test* "messages without op are ignored, connection stays usable" "7"
         (begin
           (write-uvector (bencode-encode (bencode-dict "id" "x")) raw-out)
           (write-uvector (bencode-encode (bencode-dict "id" "d" "op" "eval" "session" s1 "code" "7")) raw-out)
           (flush raw-out)
           (find-key (raw-read-all 2) "value")))
  (socket-close raw)

  (test-section "resilience")
  (test* "malformed bencode closes only that connection" #t
         (let* ([bad (make-client-socket 'inet "127.0.0.1" port)]
                [out (socket-output-port bad)])
           (display "xyz" out) (flush out)
           (let1 got (read-uvector <u8vector> 16 (socket-input-port bad))
             (socket-close bad)
             (and (eof-object? got)
                  (equal? "3" (find-key (nrepl-eval client "3" s1) "value"))))))

  (test* "responses for other ids are kept for later requests" '("10" "20")
         (let* ([m1 (bencode-dict "id" "p1" "op" "eval" "session" s1 "code" "10")]
                [m2 (bencode-dict "id" "p2" "op" "eval" "session" s1 "code" "20")]
                [out (socket-output-port (~ client 'socket))])
           ;; Send p2 behind the client's back so its responses arrive while p1 is awaited.
           (write-uvector (bencode-encode m2) out) (flush out)
           (let* ([r1 (nrepl-send client m1)]
                  [r2 (nrepl-send client (bencode-dict "id" "p2" "op" "describe"))])
             (list (find-key r1 "value") (find-key r2 "value")))))

  (test* "pending request fails when the connection drops" (test-error <error>)
         (let* ([silent (car (make-server-sockets "127.0.0.1" 0 :reuse-addr? #t))]
                [c (nrepl-connect (sockaddr-port (socket-address silent)))]
                [accepted (socket-accept silent)])
           (socket-close accepted)
           (socket-close silent)
           (nrepl-send c (bencode-dict "op" "clone"))))

  (test* "disconnected client refuses to send" (test-error <error>)
         (let* ([c (nrepl-connect port)])
           (nrepl-disconnect c)
           (nrepl-send c (bencode-dict "op" "clone"))))

  (nrepl-disconnect client)

  (test-section "server process survives hostile code")
  (let* ([p (run-process `("gosh" "-I" ,(sys-normalize-pathname "lib" :absolute #t :canonicalize #t)
                                "bin/nrepl-server" "0")
                         :output :pipe :error :null)]
         [banner (read-line (process-output p))]
         [pport (string->number (rxmatch-substring (#/port (\d+)/ banner) 1))])
    (test* "banner announces the port" #t (and pport (> pport 0)))
    (let* ([c (nrepl-connect pport)]
           [s (nrepl-clone c)])
      (for-each
       (^[code]
         (nrepl-eval c code s)
         (test* #"still alive after ~code" "2"
                (find-key (nrepl-eval c "(+ 1 1)" s) "value")))
       '("(exit 0)"
         "(error \"nope\")"
         "(raise 'symbol)"
         "(thread-start! (make-thread (^[] (error \"in thread\")))) 1"))
      (nrepl-disconnect c))
    (process-kill p)
    (process-wait p))

  (nrepl-server-stop! server)
  (test-end :exit-on-failure #t)
  0)
