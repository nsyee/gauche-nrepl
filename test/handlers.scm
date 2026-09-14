(use gauche.test)
(use srfi-1)
(test-start "nrepl.handlers")
(use nrepl.bencode)
(use nrepl.handlers)
(test-module 'nrepl.handlers)

(define ctx (make-server-context))
(define route (route-message ctx))

(define (alist r) (sort (hash-table->alist r) string<? car))
(define (find-key rs key) (any (cut dict-ref <> key) rs))
(define (last-status rs) (dict-ref (last rs) "status"))
(define (msg . kvs) (apply bencode-dict kvs))

(test-section "clone")
(define session
  (let1 rs (route (msg "id" "1" "op" "clone"))
    (test* "one response with done" '("done") (last-status rs))
    (test* "echoes id" "1" (dict-ref (car rs) "id"))
    (dict-ref (car rs) "new-session")))
(test* "new-session is a uuid" #t
       (boolean (#/^[0-9a-f-]{36}$/ session)))
(test* "session is registered" #t
       (boolean (hash-table-get (server-context-sessions ctx) session #f)))

(test-section "eval")
(define (ev code :optional (s session))
  (route (msg "id" "2" "op" "eval" "session" s "code" code)))

(test* "value then done" `((("id" . "2") ("session" . ,session) ("value" . "3"))
                           (("id" . "2") ("session" . ,session) ("status" "done")))
       (map alist (ev "(+ 1 2)")))

(test* "bindings persist within a session" "42"
       (begin (ev "(define answer 42)") (find-key (ev "answer") "value")))
(test* "multiple forms return the last value" "6"
       (find-key (ev "(define y 5) (+ y 1)") "value"))
(test* "strings are written, not displayed" "\"hi\""
       (find-key (ev "\"hi\"") "value"))
(test* "stdout is captured into out" "hello"
       (find-key (ev "(display \"hello\")") "out"))
(test* "runtime error" '("eval-error" ("done"))
       (let1 rs (ev "(car 1)")
         (list (car (dict-ref (find (cut dict-ref <> "err") rs) "status"))
               (last-status rs))))
(test* "error message names the condition" #t
       (boolean (#/pair required/ (find-key (ev "(car 1)") "err"))))
(test* "read error is reported as eval-error" #t
       (boolean (#/read-error/ (find-key (ev "(+ 1") "err"))))
(test* "exit is not available in a session" #t
       (boolean (#/exit is not available/ (find-key (ev "(exit 0)") "err"))))
(test* "empty code just completes" '("done")
       (last-status (route (msg "id" "3" "op" "eval" "session" session))))
(test* "unknown session" '("error" "unknown-session" "done")
       (last-status (ev "1" "nope")))
(test* "missing session" '("error" "unknown-session" "done")
       (last-status (route (msg "id" "4" "op" "eval" "code" "1"))))

(test-section "session isolation")
(define other (dict-ref (car (route (msg "id" "5" "op" "clone"))) "new-session"))
(test* "bindings do not leak between sessions" #t
       (boolean (#/unbound variable: answer/ (find-key (ev "answer" other) "err"))))

(test-section "close")
(test* "close status" '("session-closed" "done")
       (last-status (route (msg "id" "6" "op" "close" "session" other))))
(test* "closed session is gone" '("error" "unknown-session" "done")
       (last-status (ev "1" other)))
(test* "close without session still completes" '("session-closed" "done")
       (last-status (route (msg "id" "7" "op" "close"))))

(test-section "describe")
(let1 rs (route (msg "id" "8" "op" "describe"))
  (test* "lists supported ops" '("clone" "close" "describe" "eval")
         (dict-keys (dict-ref (car rs) "ops")))
  (test* "reports gauche version" (gauche-version)
         (dict-ref (dict-ref (dict-ref (car rs) "versions") "gauche") "version-string"))
  (test* "done" '("done") (last-status rs)))

(test-section "unknown op")
(test* "unknown-op status" '("error" "unknown-op" "done")
       (last-status (route (msg "id" "9" "op" "frobnicate"))))
(test* "message without id gets empty id" ""
       (dict-ref (car (route (msg "op" "describe"))) "id"))

(test-end :exit-on-failure #t)
